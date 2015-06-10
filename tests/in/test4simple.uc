class Main {
    y : Int ;
    x : Int <- 2 ;
    i : Int <- 0;

    main () : Int {
      while i < 5 loop {
        out_int(i);
        i <- i + 1
      } pool
    };
}
