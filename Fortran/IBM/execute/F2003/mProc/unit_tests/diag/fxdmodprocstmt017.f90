!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: fxdmodprocstmt017.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Sept. 22, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : generalization of module procedure
!*                               stmts, by making the MODULE keyword
!*                               optional. These statements are called
!*                               procedure statements in F2003.
!*
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : A procedure declaration statement inside
!*                               an interface block must not be allowed.
!*                               In other words, make sure the procedure
!*                               declaration is not recognized as proc stmt.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none

      interface
         procedure :: s ! declaration not a proc stmt. so flag it
      end interface

      interface gen1
         procedure :: s1 ! same as above, except in a generic iface
      end interface

      interface gen2
         procedure(s) s2 ! another way to declare a procedure
      end interface

      interface gen3
         procedure, nopass, s3
      end interface

      interface gen4
         procedure() s3
      end interface

      end
