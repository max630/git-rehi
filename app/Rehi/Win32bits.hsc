{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Rehi.Win32bits where

import Data.Int
import Data.Word
import Foreign.Ptr(Ptr(), nullPtr)
import Foreign.Marshal.Alloc (allocaBytesAligned, alloca)
import Foreign.Storable
import Numeric (showHex)

import qualified System.Win32.Types as WT

#include <windef.h>
#include <winnt.h>
#include <ntdef.h>

#if defined(i386_HOST_ARCH)
#let WINDOWS_CCONV = "stdcall"
#elif defined(x86_64_HOST_ARCH)
#let WINDOWS_CCONV = "ccall"
#else
# error Unknown mingw32 arch
#endif

#def typedef struct __PUBLIC_OBJECT_TYPE_INFORMATION {
      UNICODE_STRING TypeName;
      ULONG Reserved [22];
  } PUBLIC_OBJECT_TYPE_INFORMATION;

#def typedef enum {
    ObjectNameInformation = 1,
  } OBJECT_INFORMATION_CLASS;

#def NTSTATUS NtQueryObject(
  HANDLE                   Handle,
  OBJECT_INFORMATION_CLASS ObjectInformationClass,
  *PUBLIC_OBJECT_TYPE_INFORMATION  ObjectInformation,
  ULONG                    ObjectInformationLength,
  PULONG                   ReturnLength
 );

-- https://stackoverflow.com/a/8354582/2303202
-- https://wiki.haskell.org/FFICookBook#Working_with_structs
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

data NT_OBJECT_NAME_INFORMATION = NT_OBJECT_NAME_INFORMATION
    { noniLength :: WT.USHORT
    , noniMaximumLength :: WT.USHORT
    , noniBuffer :: WT.LPWSTR }

instance Storable NT_OBJECT_NAME_INFORMATION where
  sizeOf = const #{size PUBLIC_OBJECT_TYPE_INFORMATION}
  alignment = const #{alignment PUBLIC_OBJECT_TYPE_INFORMATION}
  peek p = NT_OBJECT_NAME_INFORMATION
            <$> #{peek UNICODE_STRING, Length} p
            <*> #{peek UNICODE_STRING, MaximumLength} p
            <*> #{peek UNICODE_STRING, Buffer} p
  poke p o = do
    #{poke UNICODE_STRING, Length} p (noniLength o)
    #{poke UNICODE_STRING, MaximumLength} p (noniMaximumLength o)
    #{poke UNICODE_STRING, Buffer} p (noniBuffer o)

type ObjectInformationClass = #{type OBJECT_INFORMATION_CLASS}

#enum ObjectInformationClass, , hs_ObjectNameInformation = ObjectNameInformation

type family Unsigned t :: *
type instance Unsigned Int32 = Word32

type NTSTATUS = Unsigned #{type NTSTATUS}

type ULONG = WT.DWORD

foreign import #{WINDOWS_CCONV} "NtQueryObject"
  c_NtQueryObject :: WT.HANDLE
                  -> ObjectInformationClass
                  -> Ptr NT_OBJECT_NAME_INFORMATION
                  -> ULONG
                  -> Ptr ULONG
                  -> IO NTSTATUS

getFileNameInformation :: WT.HANDLE -> IO String
getFileNameInformation h =
  alloca $ \ (p_len :: Ptr ULONG) -> do
    checkNtStatus
      (== 0xC0000004) -- STATUS_INFO_LENGTH_MISMATCH
      $ c_NtQueryObject h hs_ObjectNameInformation nullPtr 0 p_len
    len <- peek p_len
    allocaBytesAligned
        (fromIntegral len)
        (alignment (undefined :: NT_OBJECT_NAME_INFORMATION))
        $ \ p_oni -> do
      checkNtStatus
        (\s -> s >= 0 && s <= 0x7FFFFFFF) -- https://msdn.microsoft.com/en-us/library/windows/hardware/ff565436.aspx
        $ c_NtQueryObject h hs_ObjectNameInformation p_oni len p_len
      res <- peek p_len
      oni <- peek p_oni
      WT.peekTStringLen (noniBuffer oni, fromIntegral $ noniLength oni `div` 2)
  where
    checkNtStatus :: (NTSTATUS -> Bool) -> IO NTSTATUS -> IO ()
    checkNtStatus p f = do
      s <- f
      if p s
        then pure ()
        else fail ("NtQueryObject(ObjectNameInformation) failed: " ++ showHex s "")
