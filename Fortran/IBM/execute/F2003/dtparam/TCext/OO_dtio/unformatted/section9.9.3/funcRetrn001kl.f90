! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : funcRetrn001kl
!*
!*  PROGRAMMER                 : David Forster (derived from funcRetrn001 by Robert Ma)
!*  DATE                       : 2007-09-18 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.9.3: Inquire by output list
!*                               - Try output item function return of some transformational intrinsic functions
!*                                 including cshift, eoshift, merge, reshape, spread, and transpose
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   end type
end module

program funcRetrn001kl
   use m1   
     
   ! declaration of variables
   class(base(:)), allocatable :: b1(:), b2(:,:) ! tcx: (:)
   class(base(:)), pointer :: b3(:)  , b4(:,:)  ! tcx: (:)
   logical :: mergemask(2,2) = reshape( source = (/ .true. , .false. , .true. , .false. /), shape = (/2,2/) )
   integer :: length1 = 0 
   
   ! allocation of variables
   
   allocate(b1(4), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b2(2,2), source = reshape( source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi'), base(3)('jkl') /), shape=(/2,2/) )) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b3(4), source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate(b4(2,2), source = reshape( source = (/ base(3)('ABC'), base(3)('DEF'), base(3)('GHI'), base(3)('JKL') /), shape=(/2,2/) )) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   
   ! unformatted I/O operations
   
   select type ( b11 => b1 )
      type is (base(*)) ! tcx: (*)
         inquire ( iolength = length1 )              cshift (b11, shift=-1)
   end select
   
   if ( length1 /= 12 )                       error stop 101_4
   length1 = 0
   
   select type ( b13 => eoshift(b3,shift=-1, boundary=base(3)('xxx') ) ) ! tcx: (3)
      type is (base(*)) ! tcx: (*)
         inquire ( iolength = length1 )       b13    
   end select
   
   if ( length1 /= 12 )                       error stop 2_4
   length1 = 0  
     
   select type ( b12 => b2 )
      type is (base(*)) ! tcx: (*)
         select type ( b14 => b4 )
            class is (base(*)) ! tcx: (*)
               inquire ( iolength = length1 )        merge (b12,b14,mergemask)
         end select
   end select
   
   if ( length1 /= 12 )                       error stop 3_4
   length1 = 0  

   select type ( tmpb => transpose(reshape( source = b3, shape=(/2,2/) )) )
      type is (base(*)) ! tcx: (*)
         inquire ( iolength = length1 )              tmpb   
   end select
   
   if ( length1 /= 12 )                       error stop 4_4
   length1 = 0  
   
   select type ( tmpc => spread ( (/ b3 , b3 /), dim=1, ncopies=2 ) )
      type is (base(*)) ! tcx: (*)
         inquire ( iolength = length1 )              tmpc  
   end select
   
   if ( length1 /= 48 )                       error stop 5_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 25 changes
