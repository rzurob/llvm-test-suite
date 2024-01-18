!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : If a generic invocation applies to both a specific procedure
!*                               from an interface and an accessible generic intrinsic procedure,
!*                               it is the specific procedure from the interface that is referenced.
!*
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

   interface
      real(4) function deg_sin(x)
         real(4), intent(in) :: x
      end function
   end interface

   interface sin
      procedure deg_sin
   end interface

end module

real(4) function deg_sin(x)
   real(4), intent(in) :: x

   deg_sin = sin(x*0.0174532925) !<- convert degree to radian

end function

module n

   contains

      real(4) function rad_sin(x)
         real(4), intent(in) :: x
         rad_sin = sin(x)
      end function

end module

program genericAmbiguityInterface012d
   use m
   use n

   logical precision_r4

   real(4), parameter :: pi = 3.14159265

   if ( .not. precision_r4( sin(30.0), 0.5 ) ) error stop 1_4
   if ( .not. precision_r4( sin(90.0), 1.0 ) ) error stop 2_4

   if ( .not. precision_r4( rad_sin(pi/6), 0.5 ) ) error stop 3_4
   if ( .not. precision_r4( rad_sin(pi/2), 1.0 ) ) error stop 4_4

end program
