This PR adds Brave search tools with an example implementation.

## Changes

- Add Brave search integration with new tools
- Include example usage and implementation
- **Breaking Change**: Web search tool has been migrated from `web_search` to `duckduckgosearch` tool
- Parameter naming change: `query` parameter has been updated to `webAction`

## Documentation & Migration Notes

⚠️ **Action Required**: Existing prompts referencing `web_search` or the `query` parameter will need to be updated:
- Update tool name from `web_search` to `duckduckgosearch`
- Update parameter from `query` to `webAction`

This change should be documented in release notes to ensure users can migrate their integrations accordingly.