{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

#define if_(x) if(x)then do
#define else_ else do
#define include import

include Cppify
include Data.IORef
include Foreign.Marshal.Alloc
include Foreign.Ptr
import Foreign.Storable
include Prelude hiding ((.), (/), (<), (>))
include System.Exit
include System.Time


main :: IO ()
main = translationUnit


translationUnit :: IO ()
translationUnit = do

    _RAND_MAX <- static int 0x7fff;

    next <- static unsigned_int 1;

    rand_r <- function int $ \(param(ptr(unsigned_int))->(p_seed)) -> do {
        seed <- peek(p_seed);
        uncurry(poke)(p_seed, seed * 1103515245 + 12345);
        seed <- peek(p_seed);
        randmax <- readIORef(_RAND_MAX);
        res <- runInstruction$ static_cast<int>(seed % (static_cast<unsigned_int>(randmax) + 1));
        return res;
    }

    rand <- function int $ \(param()->()) -> do {
        res <- withPointer(next, rand_r);
        return res;
    }

    srand <- function void $ \(param(unsigned_int)->(seed)) -> do {
        next =: seed;
    }

    time <- function time_t $ \(param(ptr(time_t))->(timer)) -> do {
        if_(timer == 0) {
            clokc_time <- getClockTime;
            epoch <- runInstruction$ toClockTime(epoch);
            diff <- runInstruction$ uncurry(diffClockTimes)(clokc_time, epoch);
            time <- runInstruction$ toSeconds(diff);
            return (static_cast<time_t>(time));
        }
        else_{
            magic;
        }
    }

    main <- function int $ \() -> do {
        seed <- time(0);
        srand(static_cast<unsigned_int>(seed));
        n <- rand();
        randmax <- readIORef(_RAND_MAX);
        randmax2 <- runInstruction$ randmax / 2;
        if_(n <= randmax2) {
            printf("%d\n", 0);
        }
        else_{
            printf("%d\n", 1);
        };
        return 0;
    }

    exitCode <- main();
    if_(exitCode == 0) {
        exitSuccess;
    }
    else_{
        exitFailure;
    }



