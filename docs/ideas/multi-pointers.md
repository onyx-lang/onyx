Multi-Pointers
==============

&T     - Pointer to a single T
[5] T  - Array of 5 T's
[] T   - Slice of T (pointer to initial element, and a count)
[..] T - Dynamic array of T (pointer to initial element, count, capacity and allocator)
[&] T  - Pointer to many T's

This proposal would eliminate the usage of `t[x]` and `t + i` for a `t` of type `&T`. Instead,
`t` would have to be of type `[&] T` to do this. Multi-pointers would affectively
be exactly like normal pointers, but would have this ability.

I think the easiest way to implement them is by having a flag on TypePointer.
Then modify the checker to allow for the `t[x]` and `t + i` syntax.

Also, slices and dynamic arrays would have to have their internal data pointer types
become `[&] T`, so the can actually be used like arrays.

As for updating existing code, I don't think there would be too much to update. The
largest hurdle will be the code that works with `any` types, as it is common there to
cast to a `&u8` and use pointer addition. There should be/already is a better substitute
for this, but making this change would break all of that code. By my estimate, I think
that would comprise:
- `core.misc`'s any library
- `json` library
