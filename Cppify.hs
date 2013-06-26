{-# LANGUAGE CPP #-}

module Cppify where

#define include import

include Data.IORef
include Data.Word
include Foreign.Marshal.Alloc
include Foreign.Ptr
include Foreign.Storable
include Prelude hiding ((.), (/), (<), (>))
include System.Exit
include System.Time


function :: b -> (a -> IO b) -> IO (a -> IO b)
function = const(return)


(/) :: (Integral a) => a -> a -> a
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


static_cast :: a
static_cast = magic

infixl <, >

(<) :: a -> b -> b
(<) _ x = x

(>) :: (Num b, Integral a) => b -> a -> b
(>) _ x = fromIntegral(x)


epoch :: CalendarTime
epoch = CalendarTime { ctYear = 1970, ctMonth = January, ctDay = 1, ctHour = 0, ctMin = 0, ctSec = 0, ctPicosec = 0, ctWDay = Thursday, ctYDay = 0, ctTZName = "UTC", ctTZ = 0, ctIsDST = False }


toSeconds :: TimeDiff -> Time_t
toSeconds td = ((((((static_cast<time_t>(tdYear td) * 12) + static_cast<time_t>(tdMonth td) * 31) + static_cast<time_t>(tdDay td) * 24) + static_cast<time_t>(tdHour td) * 60) + static_cast<time_t>(tdMin td) * 60) + static_cast<time_t>(tdSec td * 60))


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




