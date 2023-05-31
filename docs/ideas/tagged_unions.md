Tagged Unions
-------------

Declaring a union of two types.

        Error :: union {
            io: struct { ... };
            network: struct { ... };
        }

Creating a value of that union.
Struct literal must have EXACTLY ONE NAMED member.

        err := Error.{ io = .{ ... } };

Accessing data.

        switch err {
            case .io => io_error {
                // use io_error
            }

            case .network => network_error {
                // use network_error
            }
        }

Fields.
        
        Error.tag_enum    // Implicit enum created for every union. would be enum {io, network}
        
        Error.tag_enum.io // Enum value

        err.io            // Same as Error.tag_enum.io

        cast(Error.tag_enum) err // Get the tagged value out of the union

