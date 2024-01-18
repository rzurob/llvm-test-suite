!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: 
! %GROUP: fxumodprocstmt018.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : fxumodprocstmt018
!*
!*  PROGRAMMER                 : Bardia Mahjour
!*  DATE                       : Sept. 22, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : generalization of module procedure
!*                               stmts, by making the MODULE keyword
!*                               optional. These statements are called
!*                               procedure statements in F2003.
!*                                                   
!*  SECONDARY FUNCTIONS TESTED : None 
!*
!*  DRIVER STANZA              : xlf90
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : This functional test, makes sure that
!*                               if the identifier in a procedure stmt
!*                               refers to a dummy procedure, it 
!*                               behaves as expected, when it is called
!*                               using the generic interface in which
!*                               the proc stmt appears in.
!*                               **NOTE: In this test case,
!*                               the MODULE keyword is specified, and
!*                               the expected behaviour is to ignore
!*                               this keyword when possible to recover
!*                               from a mistake made by the user.
!*
!234567890123456789012345678901234567890123456789012345678901234567890
      implicit none
      
      ! provide explicit ifaces for ss, s1, and f1
      interface
         subroutine ss(d1, d2)
           interface 
             subroutine d1()  ! dummy proc (subroutine)
             end subroutine
           end interface
           interface
              integer function d2(arg) ! dummy proc (function)
                integer :: arg
              end function
           end interface
         end subroutine ss

         subroutine s1()
         end subroutine s1
          
         integer function f1(arg)
            integer :: arg
         end function f1
      end interface
      
      call ss(s1, f1)
      
      end

      subroutine ss(ds, df)
      
      interface 
         subroutine ds()  ! dummy proc (subroutine)
         end subroutine ds
      end interface
      interface
         integer function df(arg) ! dummy proc (function)
            integer :: arg
         end function df
      end interface
       
      interface gen1
         module procedure ds
      end interface
      interface gen2
         module procedure df
      end interface

        call gen1()
        print *, gen2(3)

      end subroutine ss

      subroutine s1()
        print*, "s1"
      end subroutine s1
      
      integer function f1(arg)
         integer :: arg
         f1 = arg
      end function f1
