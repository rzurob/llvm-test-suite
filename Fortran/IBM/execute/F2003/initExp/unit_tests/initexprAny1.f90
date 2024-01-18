!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : ANY intrinsic
!*
!* DESCRIPTION                : logical type
!* ===================================================================

logical(1) :: e1T=any((/.true._1/)), e1F=any((/.false._1/))
logical(2) :: e2T=any((/.true._2/)), e2F=any((/.false._2/))
logical(4) :: e4T=any((/.true._4/)), e4F=any((/.false._4/))
logical(8) :: e8T=any((/.true._8/)), e8F=any((/.false._8/))

if (e1T .neqv. any((/.true._1/))) stop 1
if (e2T .neqv. any((/.true._2/))) stop 2
if (e4T .neqv. any((/.true._4/))) stop 3
if (e8T .neqv. any((/.true._8/))) stop 4
if (e1F .neqv. any((/.false._1/))) stop 5
if (e2F .neqv. any((/.false._2/))) stop 6
if (e4F .neqv. any((/.false._4/))) stop 7
if (e8F .neqv. any((/.false._8/))) stop 8
end
