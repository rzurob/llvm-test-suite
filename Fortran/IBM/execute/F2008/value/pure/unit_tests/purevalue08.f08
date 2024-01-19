!******************************************************************************
!*  ===========================================================================
!*
!*  DATE            : 2010-12-01
!*
!*  DESCRIPTION
!*  - array and pointer to array actual argument passed to elemental function
!*    with scalar dummy arg having value attribute
!*  - function modifies dummy arg and returns an array
!*  - caller verifies actual argument array is not modified, and the array
!*    returned
!*
!234567890123456789012345678901234567890123456789012345678901234567890123456789

integer, pointer :: parr(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
integer, target :: arr(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2)
integer :: oldarr(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2)
integer :: res(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2)
integer :: expres(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2)
integer i
arr  = reshape([(i,i=1,2*3*2*3*2*3*2*3*2*3*2*3*2)],[1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2])
oldarr = arr
expres  = arr * 2

res = foo(arr)
if (any(arr .ne. oldarr)) error stop 1
if (any(res .ne. expres)) error stop 2

parr => arr
res = foo(parr)
if (any(arr .ne. oldarr)) error stop 3
if (any(res .ne. expres)) error stop 4

contains
    integer elemental function foo ( arr )
        integer, value :: arr
        foo = arr * 2
    end function
end
