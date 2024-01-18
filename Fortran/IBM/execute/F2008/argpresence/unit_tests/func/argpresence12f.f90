!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : argpresence12f.f
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : March 2, 2011
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DRIVER STANZA              : xlf2008
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence 
!*                              function results
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 integer, pointer :: p1(:), p2(:)
 integer, target :: t1(2)
 integer, allocatable :: a1(:), a2(:) 

 p2 => t1
 allocate (a2(1))
 nullify(p1)

 call sub1(associatedpointer(),disassociatedpointer(),allocatedallocatable(),unallocatedallocatable())
 t1 = func1(associatedpointer(),disassociatedpointer(),allocatedallocatable(),unallocatedallocatable())

 contains
 subroutine sub1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z) 
 end subroutine

 integer function func1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   func1 = 1
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z) 
 end function
 function disassociatedpointer()
   integer, pointer :: disassociatedpointer(:)
 end function
 function unallocatedallocatable()
   integer, allocatable :: unallocatedallocatable(:)
 end function
 function associatedpointer()
   integer, pointer :: associatedpointer(:)
   associatedpointer => t1
 end function
 function allocatedallocatable()
   integer, allocatable :: allocatedallocatable(:)
   allocate(allocatedallocatable(2),source=[1,2])
 end function
 end
