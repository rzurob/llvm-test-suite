!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : d358226
!*  TEST CASE TITLE            : Reduced Code for Defect:  d358226
!*
!*  PROGRAMMER                 : Glen Mateer
!*  DATE                       : October 30, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  ABSTRACT                   : OOPS: ICE: DIAG: IO: IMPDO: POLY: In xlfentry
!*                               with non-DTIO Polymorphic I/O
!*
!*  DRIVER STANZA              : xlf2003
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
