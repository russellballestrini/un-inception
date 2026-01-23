// Hello World - C# with Mono (.NET Framework 4.0+)
// Compile: mcs hello-mono.cs
// Run: mono hello-mono.exe

using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Hello from C# (Mono/.NET Framework)!");
        Console.WriteLine("Runtime: " + Environment.Version);
        Console.WriteLine("Platform: " + Environment.OSVersion.Platform);
    }
}
