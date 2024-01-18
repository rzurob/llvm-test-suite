      program contiguous02f
        integer, save              :: co_ia   (9)[*]

        ! All actual arguments are simply contiguous:
        call sub_arg_coarray(co_ia,co_ia,co_ia,.TRUE.)

      contains

        subroutine sub_arg_coarray &
                          & (assumed,deferred,contig_assumed,contiguity)
          integer                               :: assumed        (:)[*]
          integer                               :: deferred       (*)[*]
          integer, contiguous                   :: contig_assumed (:)[*]
          logical                               :: contiguity

          if ( IS_CONTIGUOUS(assumed) .NEQV. contiguity ) then
            stop 1
          end if
          if ( IS_CONTIGUOUS(deferred) .NEQV. contiguity ) then
            stop 2
          end if
          if ( IS_CONTIGUOUS(contig_assumed) .NEQV. contiguity ) then
            stop 3
          end if
        end subroutine
      end
