!* ===================================================================
!*
!* DATE                       : April 19, 2007
!*
!* PRIMARY FUNCTIONS TESTED   : adjustl, adjustr intrinsics
!*
!* DESCRIPTION                : allocate and pointer with DTP
!* ===================================================================

implicit none

type base(baseLen)
   integer, len  :: baseLen
   character(baseLen), pointer :: ch(:)
   character(:), pointer :: dech(:)
end type

type(base(4)) :: base1
character(:), pointer :: c(:)
character(4), target :: tar(4)
character :: char

tar = '12'
allocate(base1%ch(10), source = ' xyz')
allocate(base1%dech(10), source = ' xyz')
c => base1%ch
char = adjustl(c(1))(4:4)
if (char /= '') error stop 1

c => base1%dech
char = adjustl(c(1))(4:4)
if (char /= '') error stop 2

base1%ch = 'abc '
char = adjustr(c(1))(1:1)
if (char /= '') error stop 3

base1%dech = 'abc '
char = adjustr(c(1))(1:1)
if (char /= '') error stop 4

base1%ch =>tar
char = adjustl(' 4'//base1%ch(1)//'3 ')(8:8)
if (char /= '') error stop 5

char = adjustr(' 4'//base1%ch(1)//'3 ')(1:1)
if (char /= '') error stop 6

base1%dech =>tar
char = adjustl(' 4'//base1%dech(1)//'3 ')(8:8)
if (char /= '') error stop 7

char = adjustr(' 4'//base1%dech(1)//'3 ')(1:1)
if (char /= '') error stop 8

end

