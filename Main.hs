{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Main where


import Data.IORef
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding ((.), (/))
import System.Exit
import System.Time

#define if_(x) if(x)then do
#define else_ else do

function :: b -> (a -> IO b) -> IO (a -> IO b)
function = const(return)


(/) = div


runInstruction :: a -> IO a
runInstruction = return


param :: a -> a -> a
param = flip(asTypeOf)


static :: a -> a -> IO (IORef a)
static = const(newIORef)


magic :: a
magic = fst(head[],tail[])


void :: ()
void = magic


int :: Int
int = magic


type Unsigned_int = Word
type Time_t = Integer


time_t :: Time_t
time_t = magic


instance Num (Ptr a) where
    fromInteger 0 = nullPtr


unsigned_int :: Unsigned_int
unsigned_int = magic


ptr :: a -> Ptr a
ptr = undefined


static_cast :: (Integral b, Num a) => a -> b -> a
static_cast _ = fromIntegral


epoch :: CalendarTime
epoch = CalendarTime { ctYear = 1970, ctMonth = January, ctDay = 1, ctHour = 0, ctMin = 0, ctSec = 0, ctPicosec = 0, ctWDay = Thursday, ctYDay = 0, ctTZName = "UTC", ctTZ = 0, ctIsDST = False }


toSeconds :: TimeDiff -> Time_t
toSeconds td = ((((((static_cast(time_t)(tdYear td) * 12) + static_cast(time_t)(tdMonth td) * 31) + static_cast(time_t)(tdDay td) * 24) + static_cast(time_t)(tdHour td) * 60) + static_cast(time_t)(tdMin td) * 60) + static_cast(time_t)(tdSec td * 60))


sizeof = const()


withPointer :: (Storable a) => (IORef a, Ptr a -> IO b) -> IO b
withPointer(var, funcptr) = do
    val <- readIORef(var)
    ptr <- malloc.sizeof(val)
    uncurry(poke)(ptr, val)
    res <- funcptr(ptr)
    val <- peek(ptr)
    free(ptr)
    var =: val
    return res


(%) :: (Integral a) => a -> a -> a
(%) = mod

(.) :: a -> b -> a
(.) = const

(=:) :: IORef a -> a -> IO ()
(=:) = writeIORef

printf :: (Show a) => (String, a) -> IO ()
printf(_,x) = print(x)


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
        res <- runInstruction$ static_cast(int)(seed % (static_cast(unsigned_int)(randmax) + 1));
        return res;
    }

    rand <- function int $ \(param()->()) -> do {
        return =<< withPointer(next, rand_r);
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
            return (static_cast(time_t)(time));
        }
        else_{
            magic;
        }
    }

    main <- function int $ \() -> do {
        seed <- time(0);
        srand(static_cast(unsigned_int)(seed));
        n <- rand();
        randmax <- readIORef(_RAND_MAX);
        randmax2 <- runInstruction$ randmax / 2;
        if_(n < randmax2) {
            printf("%d\n", 0);
        }
        else_{
            printf("%d\n", 1);
        };
        return 0;
    }

    exitCode <- main()
    if_(exitCode == 0) {
        exitSuccess;
    }
    else_{
        exitFailure;
    }



