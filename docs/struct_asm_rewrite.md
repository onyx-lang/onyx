Rewriting Structure Assembly Code
=================================

Current in the Onyx code generator, structures r-values are represented
in the execution stack as each of their individual primitive memebers.
This solution has proved relatively easy to implement, but takes A LOT
of extra instructions compared to the alternative.

I would like to rewrite the way structure r-values are handled in the
assembly to the following: all structure r-values are stored in the stack
(or somewhere in memory, but probably the stack), and the execution stack
simply contains a pointer to that address. This would allow for a structure
load/store operation to simply be replaced by a COPY, instead of the potentially
hundreds of instructions that it takes now.


The largest issue that I have thought of so far with making this change
is a flaw in how fixed-size arrays are being handled, which in turn would
be a flaw with new structure r-values as well. This code example show it:

        main :: () {
            r := array_2(array_1(.[6, 7, 8, 9, 10]));
            println(r);
        }

        array_1 :: (x: [5] u32) -> [5] u32 {
            return .[ 1, 2, 3, 4, 5 ];
        }

        array_2 :: (x: [5] u32) -> [5] u32 {
            y: [5] u32;
            for^ y do *it = 1337;

            printf("{} == {}\n", ^y, cast(^u32) x);

            return x;
        }

Here, you would expect the print in `main` to be "[1, 2, 3, 4, 5]". However,
because of a bug with where arrays are stored, you actually see "[1337, 1337, ...]".
This bug is that because array literals are living in the stack space of the
current function, returning then is the same thing as returning a pointer to
a local variable. This works in the easiest case as show below, because the
array is copied immediately into the result variable, which lives in `main`s
stack, but not when anything more complicated is going on:
        
        main :: () {
            result := array_1(.[]);
            println(result);
        }

This bug is easy to get around when working with fixed-size arrays, which is
something I do rarely anyway. However, structures are different story. Chaining
calls that return structures is done every where in Onyx programs. Just take the
`core.iter` package as an example.

        main :: () {
            for iter.as_iter(1 .. 20)
                |> iter.map(x => x * 2)
                |> iter.filter(x => x > 4)
                |> iter.take(5) {

                println(it);
            }
        }

In this example, each of the return values of those functions is an `Iterator(i32)`,
which is obviously a structure. In the new system, passing each result as a paramter
to the next function would not work as shown with the array example above. A solution
to this problem must be found before this refactor can be completed.





SOLUTION
========

I was very dumb when I wrote the above text.

This is actually very simple, and already something that the compiler is doing. I am
just doing it wrong in the fixed-size array case. The rule has to be: ALL structure
arguments need to be copied into a buffer for ARGUMENTS to the call. This already
happens with non-simple structure. Currently, it does not happen for array, so that
is why I thought this was a larger issue. The optimization that l-value structures
passed by value can simply be their pointer and not-copied can be done in the future,
but to get this optimization working, that does not have to be done.
