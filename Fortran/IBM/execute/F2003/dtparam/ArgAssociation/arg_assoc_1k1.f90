!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : F2003/dtparam/ArgAssociation/arg_assoc_1k1.f
!*  DATE                       : Mar. 23, 2009
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232
!*
!*  DESCRIPTION:
!*
!*  12.4.1 1 Actual arguments, dummy arguments, and argument association
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    type base(n)
        integer, len :: n
        character(:), allocatable :: a
        integer :: b(n*3)
    end type

    type dt(n)
        integer, len :: n
        integer, allocatable :: dta(:)
        character(n), allocatable :: dtb(:)

        type(base(:)), allocatable :: compa
        character(3), pointer :: dtc
        type(base(n)), pointer :: compb
    end type
end module

use m
implicit none
character(3), target :: tar = 'rst'
integer i
type(base(:)), allocatable, target :: x
type(dt(:)), allocatable :: y
allocate(base(3) :: x)
allocate(character(3) :: x%a)
x%a = 'abc'
x%b = [(i,i=1,9)]

allocate(dt(3)::y)
allocate(y%dta(3),y%dtb(3))
allocate(base(3) :: y%compa)
allocate(character(3) :: y%compa%a)
y%dta = [(i,i=1,3)]
y%dtb = 'xyz'
y%compa = base(3)('abc',[(i,i=1,9)])
y%dtc => tar
y%compb => x

print *,'before sub:'
print *,x%a,'-',x%b,'-',y%dta,'-',y%dtb,'-',y%dtc
print *,y%compb%a
print *,y%compb%b

call sub(x,y,3)

print *,'after sub:'
print *,x%a,'-',x%b,'-',y%dta,'-',y%dtb,'-',y%dtc
print *,y%compb%a
print *,y%compb%b

contains
    subroutine sub(x,y,n)
        use m
        integer, intent(in) :: n
        type(base(n)), target :: x
        type(dt(*)), target :: y
        type(base(n)), pointer :: p1
        type(dt(:)), pointer :: p2
        character(3), target :: autotar = 'RST'

        p1 => x
        p1%a = 'ABC'
        p1%b = [(i,i=11,19)]

        p2 => y
        p2%dta = [(i,i=11,13)]
        p2%dtb = 'XYZ'
        p2%compa = base(3)('ABC',[(i,i=11,19)])
        p2%dtc => autotar
        p2%compb => p1
    end subroutine
end
