# Questions

## How to force line breaking in strings?

By default, tokens such as strings and comments are not broken, even when extending past the line width limit.

You can control this behavior by setting:
```
CodeFormatter`BreakLines`$AllowSplittingTokens = True;
```

Now tokens will be split if the line width limit is hit in the middle of a token.






## How to change line width of formatter in Sublime? The default of 78 is way too small.

You can run arbitrary code in the command, so edit the command in your settings to something like:

```
{
"lsp_server_command":
  [
    "`kernel`",
    "-noinit",
    "-noprompt",
    "-nopaclet",
    "-nostartuppaclets",
    "-noicon",
    "-run",
    "Needs[\"LSPServer`\"];CodeFormatter`Private`$DefaultLineWidth=120;LSPServer`StartServer[]"
  ]
}
```
