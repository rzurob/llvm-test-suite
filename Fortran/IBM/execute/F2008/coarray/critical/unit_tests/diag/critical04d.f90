!*  ===================================================================
!*
!*  DATE                       : Oct 13, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : CRITICAL Construct
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 CRITICAL Construct
!*                               Testing: 8.1.5 CRITICAL construct
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program critical04d

      integer do_loop
      integer lockable

      ! 8.1.2.1 Transfer of control to the interior of a block from outside the
      !         block is prohibited
      goto 100
      critical
100     print *, "in critical"
      end critical

      ! C811 A branch (8.2) within a CRITICAL construct shall not have a branch
      !      target that is outside the construct.

      critical
        goto 200
      end critical
200   print *, "broken out!"

      ! 8.1.5.2 Execution of the CRITICAL construct is completed when execution
      !         of its block is completed. A procedure invoked, directly or
      !         indirectly, from a CRITICAL construct shall not execute an image
      !         control statement


      ! R831 cycle-stmt   is   CYCLE [ do-construct-name ]
      ! C821 (R831) A cycle-stmt shall not appear within a CRITICAL or DO
      !             CONCURRENT construct if it belongs to an outer construct.

      critical
        do do_loop=1,10
          cycle
          print *, do_loop
        end do
      end critical

      do do_loop=1,10
        critical
          cycle
          print *, do_loop
        end critical
      end do

      ! R850 exit-stmt   is   EXIT [ construct-name ]
      ! C845 An exit-stmt shall not appear within a CRITICAL or DO CONCURRENT
      !      construct if it belongs to that construct or an outer construct.
      crit_exit : critical
        exit crit_exit
      end critical crit_exit

      critical
        b_in_c : block
          exit b_in_c
        end block b_in_c
      end critical

      b : block
        c : critical
          exit b
        end critical c
      end block b

      deep_nest : block
        d_c : critical
          b1 : block
          b2 : block
          b3 : block
          b4 : block
          b5 : block
          b6 : block
          b7 : block
            exit b1        ! Valid
            exit d_c       ! Invalid
            exit deep_nest ! Invalid
          end block b7
          end block b6
          end block b5
          end block b4
          end block b3
          end block b2
          end block b1
        end critical d_c
      end block deep_nest


      contains
      ! C810 (R810) The block of a critical-construct shall not contain a RETURN
      !             statement or an image control statement.
      subroutine return_in_critical()
        critical
          return
        end critical
      end subroutine return_in_critical
      function f_return_in_critical()
        integer :: f_return_in_critical
        f_return_in_critical = 1
        critical
          return
        end critical
      end function f_return_in_critical

      end  program critical04d


      module m_return
      ! C810 (R810) The block of a critical-construct shall not contain a RETURN
      !             statement or an image control statement.
      contains
        subroutine return_in_critical()
          critical
            return
          end critical
        end subroutine return_in_critical
        function f_return_in_critical()
          integer :: f_return_in_critical
          f_return_in_critical = 1
          critical
            return
          end critical
        end function f_return_in_critical
      end module m_return
