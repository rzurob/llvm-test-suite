!*  ===================================================================
!*
!*  PRIMARY FUNCTIONS TESTED   : Diagnostic test
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : The procedure dummy argument should not
!*                               be defined in an abstract interface block.
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
interface
   subroutine sub( func, arg1, arg2 )
      integer arg1
      integer arg2
      abstract interface                ! Error: shouldn't be defined here
         integer function func( value )
            integer value
         end
      end interface
   end
end interface
end

