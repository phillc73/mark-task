module AliasCommands {
  type Alias = {
    name: str,
    command: str
  }

  var aliases: Set[Alias]
  var inputAlias: str
  var inputCommand: str
  var expandedCommand: str

  action init = all {
    aliases' = Set(),
    inputAlias' = "",
    inputCommand' = "",
    expandedCommand' = ""
  }

  action addAlias = all {
    inputAlias != "",
    inputCommand != "",
    aliases' = union(aliases, Set({ name: inputAlias, command: inputCommand })),
    inputAlias' = "",
    inputCommand' = "",
    expandedCommand' = expandedCommand
  }

  action removeAlias = all {
    inputAlias != "",
    exists(aliases, a => a.name == inputAlias),
    aliases' = filter(aliases, a => a.name != inputAlias),
    inputAlias' = "",
    inputCommand' = inputCommand,
    expandedCommand' = expandedCommand
  }

  // Expanding an alias yields the command stored for it
  action expandAlias = all {
    inputAlias != "",
    inputCommand != "",
    exists(aliases, a => a.name == inputAlias and a.command == inputCommand),
    aliases' = aliases,
    inputAlias' = "",
    inputCommand' = "",
    expandedCommand' = inputCommand
  }

  action step = any { addAlias, removeAlias, expandAlias }

  val uniqueNames = forall(aliases, a =>
    size(filter(aliases, b => b.name == a.name)) == 1)
  val inv = uniqueNames
}
