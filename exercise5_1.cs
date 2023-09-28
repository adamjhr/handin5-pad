internal class Program
{
    private static void Main(string[] args)
    {
        int[] xs1 = {1, 3, 4, 4};
        int[] ys1 = {2};
        int[] r1 = merge(xs1, ys1);
        Console.WriteLine("[{0}]", string.Join(", ", r1));

        int[] xs2 = {12, 13};
        int[] ys2 = {1, 14};
        int[] r2 = merge(xs2, ys2);
        Console.WriteLine("[{0}]", string.Join(", ", r2));

        int[] xs3 = {2, 3, 5};
        int[] ys3 = {};
        int[] r3 = merge(xs3, ys3);
        Console.WriteLine("[{0}]", string.Join(", ", r3));

        int[] xs4 = {14, 15, 16};
        int[] ys4 = {10, 11, 12};
        int[] r4 = merge(xs4, ys4);
        Console.WriteLine("[{0}]", string.Join(", ", r4));

    }

    static int[] merge (int[] xs, int[] ys) {
        int[] merged = new int[ys.Length + xs.Length];
        int xi = 0;
        int yi = 0;
        while (xi < xs.Length || yi < ys.Length) {
            if (xi == xs.Length || xs.Length == 0) {
                merged[xi+yi] = ys[yi];
                yi++;
            } else if (yi == ys.Length  || ys.Length == 0) {
                merged[xi+yi] = xs[xi];
                xi++;
            } else if (xs[xi] > ys[yi]) {
                merged[xi+yi] = ys[yi];
                yi++;
            } else {
                merged[xi+yi] = xs[xi];
                xi++;
            }
        }
        return merged;
    }
}