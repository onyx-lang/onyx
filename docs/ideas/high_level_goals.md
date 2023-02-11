High-Level Goals
================

There are many ideas  that are wizzing around my head
when I think about the future of Onyx, and I'm having
a hard-time deciding  what needs to be done and when.
I think writing everything out and creating a (rough)
timeline will  be beneficial to me  and the future of
the language.


Optimizing OVM instructions
---------------------------
As OVM becomes used more and more because of its improved
debugging experience when compared to runtimes like Wasmer,
it would be nice if the runtime was a little faster. As
I look through the generated instructions, I see a lot of
potential for some relatively easy improvements that can
be made. This would still be a larger-endeavour, as
optimizing is not a simple task. Debug information, basic
blocks and other tricky things have to be dealt with. I
think this task would take around 2-3 weeks of focused time
to get something good. 

[Onyx Website](./website.md)
--------------
The domains for the website (https://onyxlang.io) is already
registered, I just need a good-enough website. It does not
need to be anything super-fancy, yet. Just links and basic-
documentation.


[Generated Documentation](./doc_format.md)
-----------------------


[Core Library Cleanup](./core_libraries.md)
- [ ] Add improved time functionality
  - [ ] clock_gettime
  - [ ] clock_settime
  - [ ] clock_getres
  - [ ] clock
--------------------


[Project Templates](./templated_projects.md)
------------------


[Bundling Onyx for Distribution](./shipping.md)
-------------------------------

