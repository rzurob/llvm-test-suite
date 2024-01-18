!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Intrinsic  with Derived Type Parameter
!*
!* PROGRAMMER                 : James Ren
!* DATE                       : April 19, 2007
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : adjustl, adjustr intrinsics
!*
!* DESCRIPTION                : allocate and pointer with DTP in module
!* ===================================================================
module mod
type base(baseLen)
   integer, len  :: baseLen
   character(baseLen), pointer :: ch
   character(:), pointer :: dech
end type

type (base(10)), allocatable :: baset

contains

function concat(arg, length)
   type (base(length)), allocatable :: arg
   character(:), pointer :: concat
   allocate(concat, source = arg%ch//arg%dech)
end function
end module

use mod
implicit none
type(base(4)) :: base1
character(:), pointer :: c
character(4), target :: tar
character :: char

tar = '12'
allocate(base1%ch, source = ' xyz')
allocate(base1%dech, source = ' xyz')
c => base1%ch
char = adjustl(c)(4:4)
if (char /= '') error stop 1

c => base1%dech
char = adjustl(c)(4:4)
if (char /= '') error stop 2

base1%ch = 'abc '
char = adjustr(c)(1:1)
if (char /= '') error stop 3

base1%dech = 'abc '
char = adjustr(c)(1:1)
if (char /= '') error stop 4

base1%ch =>tar
char = adjustl(' 4'//base1%ch//'3 ')(8:8)
if (char /= '') error stop 5

char = adjustr(' 4'//base1%ch//'3 ')(1:1)
if (char /= '') error stop 6

base1%dech =>tar
char = adjustl(' 4'//base1%dech//'3 ')(8:8)
if (char /= '') error stop 7

char = adjustr(' 4'//base1%dech//'3 ')(1:1)
if (char /= '') error stop 8

end
   
