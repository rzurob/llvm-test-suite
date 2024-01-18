!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: dcomp array002.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 09/28/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :	
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        array non-polymorphic abstract data-ref used in intrinsic functions
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   
   type, abstract :: base
      integer :: id
   end type
   
   type, extends(base) :: child
      real :: rid
   end type
   
end module

program array002
   use m  
   
   class(base), pointer, dimension(:) :: b1
   class(child), allocatable, target :: c11(:)
   class(child), pointer :: c12(:)
  
   allocate (c11(5))
   c12 => c11
   allocate ( b1(2), source = (/ child(1,2.0), child(3,4.0) /) )

   if ( same_type_as (b1, c11(2:3)%base) )             error stop 1_4
   if ( same_type_as (b1, c12((/1/))%base) )           error stop 2_4
   
   if ( extends_type_of ( c11%base, c11 ) )            error stop 3_4
   if ( extends_type_of ( c12(1:2)%base, c12(1:2) ) )  error stop 4_4

end program