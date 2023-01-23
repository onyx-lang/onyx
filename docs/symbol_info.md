Symbol resolution information file
==================================

- Useful for LSP, documentation


File contains:
- File definition table
    - File ID
    - File name

- Symbol definition table (16 bytes each)
    - ID
    - File ID
    - Line
    - Column

- Symbol resolution table (sorted in some way to speed up lookup) (20 bytes)
    - File ID
    - Line
    - Column
    - Length
    - Symbol ID that it resolves to

Byte respresentation of the file:
    magic bytes:
        O S Y M 0x0 0x0 0x0 0x1

    table of contents:
        file definition offset   file definition entry count
        symbol definition offset symbol definition entry count
        symbol resolution offset symbol resolution entry count

    file section:
        file defintions
            id, name (count and data)
    
    symbol definition table:
        symbol definitions
            4 bytes - symbol id
            4 bytes - file id
            4 bytes - line
            4 column - column

    symbol resolution table:
        symbol resolutions:
            4 bytes - file id
            4 bytes - line
            4 bytes - column
            4 bytes - length
            4 bytes - symbol id
