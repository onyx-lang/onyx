Symbol resolution information file
==================================

- Useful for LSP, documentation


File contains:
- File definition table
    - File ID
    - File name
    * Scope ID

- Symbol definition table (16 bytes each, sorted by Symbol ID)
    - ID
    - File ID
    - Line
    - Column
    * Documentation offset
    * Documentation length

- Symbol resolution table (sorted in some way to speed up lookup) (20 bytes)
    - File ID
    - Line
    - Column
    - Length
    - Symbol ID that it resolves to

* Scopes table (12 bytes each, sorted by Scope ID)
    * Scope ID
    * Parent Scope ID (-1 for root)
    * Symbol list start index

* Scope symbols table
    * Symbol ID (-1 for end of list)

* Documentation table
    * Long list of bytes

Byte respresentation of the file:
    magic bytes:
        O S Y M 0x0 0x0 0x0 0x1

    table of contents:
        file definition offset   file definition entry count
        symbol definition offset symbol definition entry count
        symbol resolution offset symbol resolution entry count
      * docs offset   docs length

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
