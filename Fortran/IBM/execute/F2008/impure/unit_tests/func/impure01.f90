!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : impure01f.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Paul Liu
!*  DATE                       : 2012-03-08
!*  ORIGIN                     : Compiler Development, IBM China Development Shanghai Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : impure procedures
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 917300
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION                : Fortran 2008 support for the IMPURE attribute 
!*                               for procedures.An impure elemental procedure
!*                               processes array arguments in array element order.
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

      IMPURE ELEMENTAL function accumulate(a)
        implicit none
        integer :: accumulate
        integer,intent(in) :: a

        accumulate = a + 1
        print*,"in accumulate(a) a = ",a  ! 'a' (its actual parameter is array) is used in array element order
      END

      program t_main
        implicit none
        integer a(5),b(5)

        interface iface
          IMPURE ELEMENTAL function accumulate(a)
            implicit none
            integer :: accumulate
            integer,intent(in) :: a
          end function
        end interface      
 
        DATA a /10,20,30,40,50/

        print *, "a = ",a                ! b is garbage now, and the value will be different each run, do not print b
        b = accumulate(a)                ! a is array, then accumulate() will be called repeatedly in array element order
        print *, "a = ",a,"    b = ",b

      end
