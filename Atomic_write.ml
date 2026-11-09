type event = Temp_created | File_synced | Before_rename | Dir_synced

let hook = ref (fun (_ : event) (_ : string) -> ())

let write path serialize =
  let dir = Filename.dirname path in
  let prefix = Printf.sprintf ".atomic-%d-" (Unix.getpid ()) in
  let temp, oc = Filename.open_temp_file ~temp_dir:dir prefix ".tmp" in
  let closed = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !closed then close_out_noerr oc;
      if Sys.file_exists temp then Sys.remove temp)
    (fun () ->
      !hook Temp_created path;
      (match Unix.stat path with
       | stat -> Unix.fchmod (Unix.descr_of_out_channel oc) (stat.Unix.st_perm land 0o777)
       | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ());
      serialize oc;
      flush oc;
      Unix.fsync (Unix.descr_of_out_channel oc);
      !hook File_synced path;
      close_out oc;
      closed := true;
      !hook Before_rename path;
      Unix.rename temp path;
      let dir_fd = Unix.openfile dir [Unix.O_RDONLY] 0 in
      Fun.protect ~finally:(fun () -> Unix.close dir_fd) (fun () -> Unix.fsync dir_fd);
      !hook Dir_synced path)
