# .NET CORE version

Here's a C#/.netCORE version of casthex.  It holds up really well
against the [go version][1], mainly because of the expressiveness of LINQ.
So for example, determining if the casting has any moving lines is a one-liner:

    bool changes = casting.Any(sixOrNine);

The program starts up noticeably slower than the Go, prolog, and C versions, most likely
due to JIT overhead. 

[1]: https://github.com/rwtodd/misc-go

