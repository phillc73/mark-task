module Configuration {
  type MarkdownLocation = str
  type ColorTheme = str
  type KeyboardShortcut = str

  var markdownLocation: MarkdownLocation
  var colorTheme: ColorTheme
  var shortcuts: str -> KeyboardShortcut
  var inputLocation: MarkdownLocation
  var inputTheme: ColorTheme
  var inputShortcut: str
  var inputKeys: str

  action init = all {
    markdownLocation' = "/default/location/tasks.md",
    colorTheme' = "default",
    shortcuts' = Map(),
    inputLocation' = "",
    inputTheme' = "",
    inputShortcut' = "",
    inputKeys' = ""
  }

  action updateMarkdownLocation = all {
    inputLocation != "",
    markdownLocation' = inputLocation,
    colorTheme' = colorTheme,
    shortcuts' = shortcuts,
    inputLocation' = "",
    inputTheme' = inputTheme,
    inputShortcut' = inputShortcut,
    inputKeys' = inputKeys
  }

  action updateColorTheme = all {
    inputTheme != "",
    markdownLocation' = markdownLocation,
    colorTheme' = inputTheme,
    shortcuts' = shortcuts,
    inputLocation' = inputLocation,
    inputTheme' = "",
    inputShortcut' = inputShortcut,
    inputKeys' = inputKeys
  }

  action updateShortcut = all {
    inputShortcut != "",
    inputKeys != "",
    markdownLocation' = markdownLocation,
    colorTheme' = colorTheme,
    shortcuts' = shortcuts.put(inputShortcut, inputKeys),
    inputLocation' = inputLocation,
    inputTheme' = inputTheme,
    inputShortcut' = "",
    inputKeys' = ""
  }

  action step = any { updateMarkdownLocation, updateColorTheme, updateShortcut }

  val markdownLocationValid = markdownLocation != ""
  val colorThemeValid = colorTheme != ""
  val inv = and {
    markdownLocationValid,
    colorThemeValid
  }
}
