! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/funcRetrn/funcRetrn006.f
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
! %GROUP: funcRetrn006.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
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
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic array abstract type
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
   
   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type
   
   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

contains

   class(base(4)) function foo(a)
      pointer :: foo(:)
      class(base(4)), intent(in), dimension(:) :: a
      allocate(foo(size(a)), source=a ) 
   end function
   
   function foo1(a) result (boo)
      class(base(4)), pointer, dimension(:) :: boo
      class(base(4)), intent(in), dimension(:) :: a
      allocate(boo(size(a)), source=a) 
   end function
   
end module

program funcRetrn006
   use m   
   
   class(base(4)), pointer, dimension(:) :: c
   class(base(4)), allocatable, dimension(:) :: b
   
   allocate (b(2), source = (/ child(4,4,20)(4), child(4,4,20)(5) /) )
     
   c => foo(b)
   if (c(1)%id .ne. 4) error stop 1_4
   if (c(2)%id .ne. 5) error stop 2_4
   deallocate (c)
   
   allocate ( c(2), source = foo1((/child(4,4,20)(5), child(4,4,20)(6) /)) )    
   if (c(1)%id .ne. 5) error stop 3_4     
   if (c(2)%id .ne. 6) error stop 4_4 
   
end program

