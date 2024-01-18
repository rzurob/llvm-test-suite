!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Deferred Character Length
!*
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the character intrinsics
!*                               with deferred length.
!*                               The intrinsics include:
!*                               1. POS
!*                               2. INDEX
!*                               3. SCAN
!*                               4. REPEAT
!*                               5. LEN_TRIM
!*                               6. VERIFY
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

implicit none
integer pos
integer rc/0/
character(:), allocatable :: char1, char2
character(:), pointer :: pchar(:,:)
character(10), target :: tchar(2,3)

allocate(character(15)::char1)
allocate(character(2)::char2)
char1 = '@#123?56?5>78.?'
char2 = '?5'
pchar => tchar
tchar(1,1) = '    apple'
tchar(2,1) = 'pine apple'
tchar(1,2) = 'melon'
tchar(2,2) = 'watermelon'
tchar(1,3) = 'pear  '
tchar(2,3) = 'beer pear '

pos = index(char1, char2)
if (pos .ne. 6) rc = rc + 1

pos = index(char1, char2, back = .true.)
if (pos .ne. 9) rc = rc + 2

pos = index(pchar(2,1), (pchar(1,1)(6:8)))
if (pos .ne. 7) rc = rc + 3

call zzrc(rc)

if (scan(char1, char2) .ne. 6)  rc = rc + 1

if (scan(char1, char2, back = .true.) .ne. 15) rc = rc + 2

if (scan(char1(2:9), char2) .ne. 5)  rc = rc + 3

if (scan(char1(2:9), char2, back = .true.) .ne. 8)  rc = rc + 4

if (scan(pchar(1,1), pchar(1,2)(3:3)) .ne. 8) rc = rc + 5

if (scan(pchar(2,3), pchar(2,2)(3:5)) .ne. 2) rc = rc + 6

if (scan(pchar(2,3), pchar(2,2)(3:5), back = .true.) .ne. 9) rc = rc + 7

call zzrc(rc)

if (repeat(char1(3:5), 3) /= '123123123') rc = rc + 1

if (repeat(pchar(1,3)(1:5), 2) /= 'pear pear ') rc = rc + 2

call zzrc(rc)

if (len_trim(pchar(1,3)) .ne. 4) rc = rc + 1

if (len_trim(pchar(2, 3)) .ne. 9) rc = rc + 2

call zzrc(rc)

if (verify(char1, char2) .ne. 1) rc = rc + 1

if (verify(char1(6:15), char2) .ne. 3) rc = rc + 2

if (verify(char1(6:15), char2, back = .true.) .ne. 9) rc = rc + 3

if (verify(pchar(2,3), pchar(2,2)(3:5), back = .true.) .ne. 10) rc = rc + 4

if (verify(pchar(2,1), pchar(1,3)) .ne. 2) rc = rc + 5

if (verify(pchar(2,3), pchar(2,2)(3:5)) .ne. 1) rc = rc + 6

call zzrc(rc)

deallocate(char1, char2)
end
