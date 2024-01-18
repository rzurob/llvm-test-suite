!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : CONTIGUOUS simply contiguous
!*                               6.5.4 & 12.5.2.7
!*
!*  DESCRIPTION                : Testing proper diagnostics of
!*                               the F2008 CONTIGUOUS simply contig
!*                               Testing 6.5.4, 12.5.2.7 CONTIGUOUS
!*                               C1241
!*
!234567890123456789012345678901234567890123456789012345678901234567890

      program contiguous09d

       ! C1239 (R1223) If an actual argument is a nonpointer array that
       !  has the ASYNCHRONOUS or VOLATILE attribute but is not simply
       !  contiguous (6.5.4), and the corresponding dummy argument has
       !  either the VOLATILE or ASYNCHRONOUS attribute, that dummy
       !  argument shall be an assumed-shape array that does not have
       !  the CONTIGUOUS attribute.
       integer, volatile :: iv(10)

       ! C1240 (R1223) If an actual argument is an array pointer that
       !  has the ASYNCHRONOUS or VOLATILE attribute but does not have
       !  the CONTIGUOUS attribute, and the corresponding dummy argument
       !  has either the VOLATILE or ASYNCHRONOUS attribute, that dummy
       !  argument shall be an array pointer or an assumed-shape array
       !  that does not have the CONTIGUOUS attribute.
       integer, asynchronous, pointer :: ia(:)
       integer, asynchronous, contiguous, pointer :: iac(:)

       call sub_arg_volatile(iv)         ! iv is simply contiguous
       I = 1
       call sub_arg_volatile(iv(1:10:I)) ! Not simply contiguous

       call sub_arg_asynch(iac,iac) ! Has contiguous attr
       call sub_arg_asynch(ia,ia)   ! Not as much attr

       contains
        subroutine sub_arg_volatile(assumed)
          ! C1239 (R1223) If an actual argument is a nonpointer array that
          !  has the ASYNCHRONOUS or VOLATILE attribute but is not simply
          !  contiguous (6.5.4), and the corresponding dummy argument has
          !  either the VOLATILE or ASYNCHRONOUS attribute, that dummy
          !  argument shall be an assumed-shape array that does not have
          !  the CONTIGUOUS attribute.
          integer, volatile, contiguous :: assumed (:)
        end subroutine

        subroutine sub_arg_asynch(assumed, ptr)
          ! C1240 (R1223) If an actual argument is an array pointer that
          !  has the ASYNCHRONOUS or VOLATILE attribute but does not have
          !  the CONTIGUOUS attribute, and the corresponding dummy argument
          !  has either the VOLATILE or ASYNCHRONOUS attribute, that dummy
          !  argument shall be an array pointer or an assumed-shape array
          !  that does not have the CONTIGUOUS attribute.
          integer, asynchronous, contiguous           :: assumed (:)
          integer, asynchronous, contiguous, pointer  :: ptr (:)
        end subroutine
      end
