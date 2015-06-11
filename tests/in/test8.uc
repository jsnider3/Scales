class Main {
    y : Int ;
    x : Int <- 2 ;
    print_fn(a: Int, b: Int, c: Int, d: Int, e: Int) : Int {
       out_int(a * b * c * d * e)
   };
    main () : Int {
      {y <- 9;
      x <- print_fn(42, x, y, -4, 1);
      out_int(x)
      }
    };
}
