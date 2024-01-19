!***********************************************************************
!* =====================================================================
!*
!*  DATE                       : October 30, 2008
!*
!*  ABSTRACT                   : OOPS: ICE: DIAG: IO: IMPDO: POLY: In xlfentry
!*                               with non-DTIO Polymorphic I/O
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  ORIGINAL TEST CASE         :
!*  F2003/dtparam/io/unformatted/stream/streamReadWrite08.scenario
!*
!*  DESCRIPTION                :
!*  The Reduced Code (below) ICEs in "xlfentry" after emitting a
!*  Diagnostic for a non-DTIO I/O Statement involving a Polymorphic
!*  Entity and an io-implied-do:
!*
!*  1512-138 (S) A polymorphic entity must not appear in a data transfer
!*  statement unless it is processed by a user-defined derived-type I/O
!*  procedure.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

PROGRAM d358226
    IMPLICIT NONE

    TYPE base
    END TYPE base

    CONTAINS

        SUBROUTINE ReadItems( pdt )
            CLASS(base) :: pdt( : )

            INTEGER :: i

            PRINT *, (pdt( i ), i = 1, 1)       ! <= Line 14 (Now 51)

        END SUBROUTINE ReadItems

END PROGRAM d358226
