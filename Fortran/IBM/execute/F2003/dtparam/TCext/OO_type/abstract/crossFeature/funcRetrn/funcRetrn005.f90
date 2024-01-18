! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/funcRetrn/funcRetrn005.f
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
! %GROUP: funcRetrn005.f
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
!*                                        optional polymorphic abstract type dummy argument
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

   class(base(4)) function foo(a,b)
      class(base(4)), intent(in) :: a
      class(base(4)), optional, intent(in):: b
      pointer :: foo
      if (present(b)) then
         allocate (foo, source = b)
      else
         allocate (foo, source = a)
      end if
   end function
  
end module

program funcRetrn005
   use m   

   class(base(4)), pointer :: c1
   class(base(4)), pointer :: c2
   
   allocate(c1, source=child(4,4,20)(5) )
   
   c2 => foo(c1)
   if (c2%id .ne. 5) error stop 1_4
   
   c1 => foo(c1,child(4,4,20)(7))
   if (c1%id .ne. 7) error stop 2_4
   
   allocate(c2, source = foo(child(4,4,20)(2), child(4,4,20)(3)) )
   if (c2%id .ne. 3) error stop 3_4
   
end program

