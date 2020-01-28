open Framework;
open QCheckRely;
open Generator.Fantasy;
open Lib.Troll;

let {describe} = extendDescribe(QCheckRely.Matchers.matchers);

describe("Troll Invariance", ({test}) => {

  test("Troll score should be 0 when all elves resurrected", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="Troll score should be 0 when all elves resurrected",
      troll_arbitrary,
      troll =>
      all_elves_resurrected(troll) |> scoring == 0
    )
    |> expect.ext.qCheckTest;
    ();
  });

  test("Troll score should always be >= 0", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="Troll score should always be >= 0",
      troll_arbitrary,
      troll => scoring(troll) >= 0
    )
    |> expect.ext.qCheckTest;
    ();
  });

});

describe("Troll Inverse", ({test}) => {

  test("oops_he_survived should always be inverse of i_got_one", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="oops_he_survived should always be inverse of i_got_one",
      troll_elf_arbitrary,
      ((troll,elfe)) => {
        scoring( troll |> i_got_one(elfe) |> oops_he_survived(elfe) ) == scoring(troll)
      }
    )
    |> expect.ext.qCheckTest;
    ();
  });

});

describe("Troll Analogy", ({test}) => {

  test("i_got_one and i_got should be consistent", ({expect}) => {
    QCheck.Test.make(
      ~count=1000,
      ~name="i_got_one and i_got should be consistent",
      troll_elf_int_arbitrary,
      ((troll, elf, number)) => {
        let troll2 = ref(troll);
        for (i in 1 to number) {
          troll2 := i_got_one(elf, troll2^); 
        };     
        scoring(troll2^) == scoring( i_got(number, elf, troll) );
      }
    )
    |> expect.ext.qCheckTest;
    ();
  });

});

describe("Troll Idempotence", ({test}) => {

  test(
    "all_elves_of_a_kind_resurrected brings the Troll killing list to a stable state",
    ({expect}) => {
      QCheck.Test.make(
        ~count=1000,
        ~name="all_elves_of_a_kind_resurrected brings the Troll killing list to a stable state",
  	    troll_elf_arbitrary,
        ((troll,elf))=>
        all_elves_of_a_kind_resurrected(elf,troll) == ( all_elves_of_a_kind_resurrected(elf,troll) |> all_elves_of_a_kind_resurrected(elf) |> all_elves_of_a_kind_resurrected(elf) )
      )
      |> expect.ext.qCheckTest;
      ();
    }
  );

});

describe("Troll Metamorphism", ({test}) => {

  test(
    "i_got_one doit augmenter le score du troll",
    ({expect}) => {
      QCheck.Test.make(
        ~count=1000,
        ~name="i_got_one doit augmenter le score du troll",
        troll_elf_arbitrary,
        ((troll,elf)) => scoring(i_got_one(elf, troll)) > scoring(troll)
      )
      |> expect.ext.qCheckTest;
      ();
    }
  );

  test(
    "all_elves_of_a_kind_resurrected doit diminuer le score du troll",
    ({expect}) => {
      QCheck.Test.make(
        ~count=1000,
        ~name="all_elves_of_a_kind_resurrected doit diminuer le score du troll",
        troll_elf_arbitrary,
        ((troll, elf)) => scoring(all_elves_of_a_kind_resurrected(elf,troll)) <= scoring(troll)
      )
      |> expect.ext.qCheckTest;
      ();
    }
  );

});

describe("Troll Injection", ({test}) => {

  test(
    "",
    ({expect}) => {
      QCheck.Test.make(
        ~count=1000,
        ~name="",
        troll_two_elves_arbitrary,
        ((troll, elf1, elf2)) => {
          let troll2 = i_got_one(elf1, troll);
          scoring(troll2) < scoring( i_got_one(elf2, troll2) );
        }
      )
      |> expect.ext.qCheckTest;
      ();
    }
  );
    
});