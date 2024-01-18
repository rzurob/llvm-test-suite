!*  ===================================================================
!*
!*  DATE                       : June 24, 2010
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

      module m
        integer, asynchronous, pointer, contiguous :: mipac(:)
      end module m

      module n
        integer, asynchronous, pointer, contiguous :: ipac(:)
      end module n

      program contiguous11d
        use m
        use n, nipac=>ipac

        call sub_arg_asynch(mipac,nipac) ! Has contiguous attr
        call sub_arg_asynch(nipac,mipac) ! Has contiguous attr

      contains
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
