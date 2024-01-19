! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
   type base
      character(3) :: c = ''
   end type
end module

program funcRetrn001
   use m1

   ! declaration of variables
   class(base), allocatable :: b1(:), b2(:,:)
   class(base), pointer :: b3(:)  , b4(:,:)
   logical :: mergemask(2,2) = reshape( source = (/ .true. , .false. , .true. , .false. /), shape = (/2,2/) )
   integer :: length1 = 0

   ! allocation of variables

   allocate(b1(4), source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /) )
   allocate(b2(2,2), source = reshape( source = (/ base('abc'), base('def'), base('ghi'), base('jkl') /), shape=(/2,2/) ))
   allocate(b3(4), source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /) )
   allocate(b4(2,2), source = reshape( source = (/ base('ABC'), base('DEF'), base('GHI'), base('JKL') /), shape=(/2,2/) ))

   ! unformatted I/O operations

   select type ( b11 => b1 )
      type is (base)
         inquire ( iolength = length1 )              cshift (b11, shift=-1)
   end select

   if ( length1 /= 12 )                       error stop 1_4
   length1 = 0

   select type ( b13 => eoshift(b3,shift=-1, boundary=base('xxx') ) )
      type is (base)
         inquire ( iolength = length1 )       b13
   end select

   if ( length1 /= 12 )                       error stop 2_4
   length1 = 0

   select type ( b12 => b2 )
      type is (base)
         select type ( b14 => b4 )
            class is (base)
               inquire ( iolength = length1 )        merge (b12,b14,mergemask)
         end select
   end select

   if ( length1 /= 12 )                       error stop 3_4
   length1 = 0

   select type ( tmpb => transpose(reshape( source = b3, shape=(/2,2/) )) )
      type is (base)
         inquire ( iolength = length1 )              tmpb
   end select

   if ( length1 /= 12 )                       error stop 4_4
   length1 = 0

   select type ( tmpc => spread ( (/ b3 , b3 /), dim=1, ncopies=2 ) )
      type is (base)
         inquire ( iolength = length1 )              tmpc
   end select

   if ( length1 /= 48 )                       error stop 5_4

end program
