module InteractiveMode {
  type Command = str
  type Response = str

  var commandHistory: List[Command]
  var currentCommand: Command
  var responses: List[Response]
  var inputCommand: Command

  action init = all {
    commandHistory' = List(),
    currentCommand' = "",
    responses' = List(),
    inputCommand' = ""
  }

  pure def processCommand(cmd) =
    if (cmd == "quit") "Exiting"
    else if (cmd == "help") "Help text"
    else "Unknown"

  action executeCommand = all {
    inputCommand != "",
    commandHistory' = append(commandHistory, inputCommand),
    currentCommand' = inputCommand,
    responses' = append(responses, processCommand(inputCommand)),
    inputCommand' = ""
  }

  action step = any { executeCommand }

  // Every executed command produced exactly one response
  val historyMatchesResponses = length(commandHistory) == length(responses)
  val inv = historyMatchesResponses
}
