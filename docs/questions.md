# Questions

## How to force line breaking in strings?

```
CodeFormatter`BreakLines`$AllowSplittingTokens = True;
```



## How to change line width of formatter in Sublime? The default of 78 is too small.


Change your command to something like:

```
{
"lsp_server_command":
  [
    "`kernel`",
    "-noinit",
    "-noprompt",
    "-nopaclet",
    "-noicon",
    "-run",
    "Needs[\"LSPServer`\"];CodeFormatter`Private`$DefaultLineWidth=120;LSPServer`StartServer[]"
  ]
}
```
