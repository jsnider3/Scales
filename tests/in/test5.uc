class Main {
    y : Int ;
    x : Int ;
    fibonacci(a: Int) : Int {
       if a < 2 then 1 else fibonacci(a - 1) + fibonacci(a - 2)
       fi
   };
    main () : Int {
      {y <- in_int();
      x <- fibonacci(y);
      out_int(x)
      }
    };
}
