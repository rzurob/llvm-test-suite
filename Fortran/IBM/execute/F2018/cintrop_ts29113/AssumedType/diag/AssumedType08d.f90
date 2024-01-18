!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : AssumedType08d
!*
!*  PROGRAMMER                 : Dorra Bouchiha
!*  DATE                       : June 13, 2012
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2008
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : TYPE(*) cannot appear in type-spec of 
!*                               an ALLOCATE statement
!*
!**********************************************************************
!234567890123456789012345678901234567890123456789012345678901234567890
program AssumedType08d
implicit none
class(*), allocatable :: i 
class(*), allocatable :: arr(:) 
class(*), pointer     :: p 

allocate( TYPE(*) :: i)
allocate( TYPE(*) :: p)

allocate( * :: i)
allocate( * :: arr(5))
allocate( * :: p)

end program AssumedType08d
