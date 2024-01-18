! *********************************************************************
!*  ===================================================================
!*
!*  Creation Date              : Mar 07, 2003
!*
!*  Primary Function Tested    : Unformatted stream access I/O
!*
!*  Description                : Test typeless literal constant with
!*                               stream I/O.
!*
!=======================================================================

!* Declare Variables and constants.

  implicit none
  integer id(10)
  integer i, j, k, l

!* Hexadecimal Constants
  real Ahex(3,3), Bhex(3,3)
  real arrhex1(3), arrhex2(3), arrhex3(3), arrhex10(9)
  real Acolhex1(3), Acolhex2(3), Acolhex3(3)
  real varhex1 /Z"037bf"/, varhex2
  real, parameter :: parhex(3) = (/ X'012', Z'0', z'111'/)

!* Octal Constants
  integer Aoct(3,3), Boct(3,3)
  integer arroct1(3), arroct2(3), arroct3(3), arroct10(9)
  integer Acoloct1(3), Acoloct2(3), Acoloct3(3)
  integer varoct1 /O'1276'/, varoct2
  integer, parameter :: paroct(3) = (/ O'1', o'102', '257'O/)

!* Binary Constants
  integer Abin(3,3), Bbin(3,3)
  integer arrbin1(3), arrbin2(3), arrbin3(3), arrbin10(9)
  integer Acolbin1(3), Acolbin2(3), Acolbin3(3)
  integer varbin1 /B'10101010'/, varbin2
  integer, parameter :: parbin(3) = (/ '10'b, b'1111', B'1001'/)

!* Hollerith Constants
  integer Aholl(3,3), Bholl(3,3)
  integer arrholl1(3), arrholl2(3), arrholl3(3), arrholl10(9)
  integer Acolholl1(3), Acolholl2(3), Acolholl3(3), Acolholl10(9)
  integer varholl1 /36Habcdefghijklmnopqrstuvwxyz1234567890 /, varholl2
  integer, parameter :: parholl(3) = (/ 5H12345, 10H1a2b3c4d5e, 7Hfortran/)

  logical finish /.false./

!* Test1 : Hexadecimal constants

   open(1, access='stream', form='unformatted', asynch='yes')
   write(1, id = id(1)) varhex1
   write(1, id = id(2)) parhex
   wait(id =id(1))
   wait(id =id(2))
   rewind(1)

   read(1, id=id(1)) varhex2
   read(1, id=id(2)) arrhex1

   wait(id = id(1))
   if (varhex1 .ne. varhex2) error stop 1
   wait(id = id(2))
   if (arrhex1(1) .ne. parhex(1)) error stop 11
   if (arrhex1(2) .ne. parhex(2)) error stop 12
   if (arrhex1(3) .ne. parhex(3)) error stop 13

   close(1, status='delete')

!* TEST2 : Octal constants
   open(1, access='stream', form='unformatted', asynch='yes')
   write(1, id = id(1)) varoct1
   write(1, id = id(2)) paroct
   wait(id =id(1))
   wait(id =id(2))
   rewind(1)

   read(1, id=id(1)) varoct2
   read(1, id=id(2)) arroct1

   wait(id = id(1))
   if (varoct1 .ne. varoct2) error stop 2
   wait(id = id(2))
   if (arroct1(1) .ne. paroct(1)) error stop 21
   if (arroct1(2) .ne. paroct(2)) error stop 22
   if (arroct1(3) .ne. paroct(3)) error stop 23

   close(1, status='delete')

!* TEST3 : Binary constants

   open(1, access='stream', form='unformatted', asynch='yes')
   write(1, id=id(1)) varbin1
   write(1, id=id(2)) parbin

   wait(id =id(1))
   wait(id =id(2))
   rewind(1)

   read(1, id=id(1)) varbin2
   read(1, id=id(2)) arrbin1

   wait(id = id(1))
   if (varbin1 .ne. varbin2) error stop 3
   wait(id = id(2))
   if (arrbin1(1) .ne. parbin(1)) error stop 31
   if (arrbin1(2) .ne. parbin(2)) error stop 32
   if (arrbin1(3) .ne. parbin(3)) error stop 33

   close(1, status='delete')

!* TEST4 : Hollerith
   open(1, access='stream', form='unformatted', asynch='yes')
   write(1, id=id(1)) varholl1
   write(1, id=id(2)) parholl

   wait(id =id(1))
   wait(id =id(2))
   rewind(1)

   read(1, id=id(1)) varholl2
   read(1, id=id(2)) arrholl1

   wait(id = id(1))
   if (varholl1 .ne. varholl2) error stop 4
   wait(id = id(2))
   if (arrholl1(1) .ne. parholl(1)) error stop 41
   if (arrholl1(2) .ne. parholl(2)) error stop 42
   if (arrholl1(3) .ne. parholl(3)) error stop 43

   close(1, status='delete')

end

