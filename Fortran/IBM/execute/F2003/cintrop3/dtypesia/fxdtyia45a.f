!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived types with BIND(C) attribute
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - Testing arrays of 3-levels deep derived types with BIND(C) attribute
!*      - Testing arrays of 3-levels deep derived types with INTENT attributes
!*      - Testing arrays of 3-levels deep derived types with integer components
!*      - Testing FORTRAN external functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxdtyia45
   use ISO_C_BINDING

   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(1) var_a(DIM1,DIM2)
      integer(C_INT) var_b(DIM1,DIM2)
      integer(C_SHORT) var_c(DIM1,DIM2)
      integer(8) var_d(DIM1,DIM2)
      integer(C_LONG) var_e(DIM1,DIM2)
      integer(2) var_f(DIM1,DIM2)
      integer(C_LONG_LONG) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt2
      integer(C_INT16_T) var_a(DIM1,DIM2)
      integer(C_INT_FAST8_T) var_b(DIM1,DIM2)
      integer(C_INT64_T) var_c(DIM1,DIM2)
      integer(C_INT8_T) var_d(DIM1,DIM2)
      integer(C_INT_FAST32_T) var_e(DIM1,DIM2)
      integer(C_INT_FAST16_T) var_f(DIM1,DIM2)
      integer(C_INT32_T) var_g(DIM1,DIM2)
      integer(C_INT_FAST64_T) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2(DIM2,DIM1)
      integer(C_SIZE_T) var_a(DIM1,DIM2)
      integer(C_INT_LEAST8_T) var_b(DIM1,DIM2)
      integer(C_INT_LEAST64_T) var_c(DIM1,DIM2)
      integer(C_SIGNED_CHAR) var_d(DIM1,DIM2)
      integer(C_INTPTR_T) var_e(DIM1,DIM2)
      integer(C_INT_LEAST16_T) var_f(DIM1,DIM2)
      integer(C_INTMAX_T) var_g(DIM1,DIM2)
      integer(C_INT_LEAST32_T) var_h(DIM1,DIM2)
   end type

end module mxdtyia45

module auxmod
   use mxdtyia45

   interface operator(+)
      module procedure adddty_s, adddty_d, adddty_t
   end interface

contains

   elemental function adddty_s(dtx,dty)
      type(dt1), intent(in) :: dtx, dty
      type(dt1) :: adddty_s

      do i = 1, DIM1
       do j = 1, DIM2
         adddty_s%var_a(i,j) = dtx%var_a(i,j) + dty%var_a(i,j)
         adddty_s%var_b(i,j) = dtx%var_b(i,j) + dty%var_b(i,j)
         adddty_s%var_c(i,j) = dtx%var_c(i,j) + dty%var_c(i,j)
         adddty_s%var_d(i,j) = dtx%var_d(i,j) + dty%var_d(i,j)
         adddty_s%var_e(i,j) = dtx%var_e(i,j) + dty%var_e(i,j)
         adddty_s%var_f(i,j) = dtx%var_f(i,j) + dty%var_f(i,j)
         adddty_s%var_g(i,j) = dtx%var_g(i,j) + dty%var_g(i,j)
         adddty_s%var_h(i,j) = dtx%var_h(i,j) + dty%var_h(i,j)
       end do
      end do

   end function adddty_s

   elemental function adddty_d(dtx,dty)
      type(dt2), intent(in) :: dtx, dty
      type(dt2) :: adddty_d

      do i = 1, DIM1
       do j = 1, DIM2
         adddty_d%var_a(i,j) = dtx%var_a(i,j) + dty%var_a(i,j)
         adddty_d%var_b(i,j) = dtx%var_b(i,j) + dty%var_b(i,j)
         adddty_d%var_c(i,j) = dtx%var_c(i,j) + dty%var_c(i,j)
         adddty_d%var_d(i,j) = dtx%var_d(i,j) + dty%var_d(i,j)
         adddty_d%var_e(i,j) = dtx%var_e(i,j) + dty%var_e(i,j)
         adddty_d%var_f(i,j) = dtx%var_f(i,j) + dty%var_f(i,j)
         adddty_d%var_g(i,j) = dtx%var_g(i,j) + dty%var_g(i,j)
         adddty_d%var_h(i,j) = dtx%var_h(i,j) + dty%var_h(i,j)
       end do
      end do
      adddty_d%vdt1 = adddty_s(dtx%vdt1,dty%vdt1)

   end function adddty_d

   elemental function adddty_t(dtx,dty)
      type(dt3), intent(in) :: dtx, dty
      type(dt3) :: adddty_t

      do i = 1, DIM1
       do j = 1, DIM2
         adddty_t%var_a(i,j) = dtx%var_a(i,j) + dty%var_a(i,j)
         adddty_t%var_b(i,j) = dtx%var_b(i,j) + dty%var_b(i,j)
         adddty_t%var_c(i,j) = dtx%var_c(i,j) + dty%var_c(i,j)
         adddty_t%var_d(i,j) = dtx%var_d(i,j) + dty%var_d(i,j)
         adddty_t%var_e(i,j) = dtx%var_e(i,j) + dty%var_e(i,j)
         adddty_t%var_f(i,j) = dtx%var_f(i,j) + dty%var_f(i,j)
         adddty_t%var_g(i,j) = dtx%var_g(i,j) + dty%var_g(i,j)
         adddty_t%var_h(i,j) = dtx%var_h(i,j) + dty%var_h(i,j)
       end do
      end do
      adddty_t%vdt2 = adddty_d(dtx%vdt2,dty%vdt2)

   end function adddty_t

end module auxmod

program fxdtyia45a
   use mxdtyia45
   use auxmod
   interface
      function fun1(dt) bind(c)
         import dt3, C_PTR, DIM1, DIM2
         type(dt3), dimension(DIM2,DIM1) :: dt
         type(C_PTR) :: fun1
      end function fun1
      function fun3(dtx,dty) bind(c)
         import dt3, C_PTR, DIM1, DIM2
         type(dt3), dimension(DIM2,DIM1), intent(in) :: dtx
         type(dt3), dimension(DIM2,DIM1), intent(out) :: dty
         type(C_PTR) :: fun3
      end function fun3
   end interface

   type(dt1), parameter :: dt01 = dt1( &
              reshape((/1,2,3,4,5,6/),(/DIM1,DIM2/)),&
              reshape((/2,3,4,5,6,7/),(/DIM1,DIM2/)),&
              reshape((/3,4,5,6,7,8/),(/DIM1,DIM2/)),&
              reshape((/4,5,6,7,8,9/),(/DIM1,DIM2/)),&
              reshape((/5,6,7,8,9,10/),(/DIM1,DIM2/)),&
              reshape((/6,7,8,9,10,11/),(/DIM1,DIM2/)),&
              reshape((/7,8,9,10,11,12/),(/DIM1,DIM2/)),&
              reshape((/8,9,10,11,12,13/),(/DIM1,DIM2/)))

   type(dt2), parameter :: dt02 = dt2( &
              reshape((/1,2,3,4,5,6/),(/DIM1,DIM2/)),&
              reshape((/2,3,4,5,6,7/),(/DIM1,DIM2/)),&
              reshape((/3,4,5,6,7,8/),(/DIM1,DIM2/)),&
              reshape((/4,5,6,7,8,9/),(/DIM1,DIM2/)),&
              reshape((/5,6,7,8,9,10/),(/DIM1,DIM2/)),&
              reshape((/6,7,8,9,10,11/),(/DIM1,DIM2/)),&
              reshape((/7,8,9,10,11,12/),(/DIM1,DIM2/)),&
              reshape((/8,9,10,11,12,13/),(/DIM1,DIM2/)),&
              reshape((/dt01,dt01,dt01,dt01,dt01,dt01/),(/DIM2,DIM1/)))

   type(dt3), parameter :: dt0 = dt3( &
              reshape((/dt02,dt02,dt02,dt02,dt02,dt02/),(/DIM2,DIM1/)),&
              reshape((/1,2,3,4,5,6/),(/DIM1,DIM2/)),&
              reshape((/2,3,4,5,6,7/),(/DIM1,DIM2/)),&
              reshape((/3,4,5,6,7,8/),(/DIM1,DIM2/)),&
              reshape((/4,5,6,7,8,9/),(/DIM1,DIM2/)),&
              reshape((/5,6,7,8,9,10/),(/DIM1,DIM2/)),&
              reshape((/6,7,8,9,10,11/),(/DIM1,DIM2/)),&
              reshape((/7,8,9,10,11,12/),(/DIM1,DIM2/)),&
              reshape((/8,9,10,11,12,13/),(/DIM1,DIM2/)))

   type(dt3), dimension(DIM2,DIM1) :: dta, dtb
   type(dt3), pointer :: dtr(:,:)
   type(C_PTR) :: dtp

!! Test 1

   dta = reshape((/dt0,dt0,dt0,dt0,dt0,dt0/),(/DIM2,DIM1/))

   dtp = fun1(dta)

   call C_F_POINTER(dtp,dtr,(/DIM2,DIM1/))

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dta(l,k)%var_a(i,j) /= i+j+(j-1)*DIM2 ) error stop 20
       if ( dta(l,k)%var_b(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 22
       if ( dta(l,k)%var_c(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 24
       if ( dta(l,k)%var_d(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 26
       if ( dta(l,k)%var_e(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 28
       if ( dta(l,k)%var_f(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 30
       if ( dta(l,k)%var_g(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 32
       if ( dta(l,k)%var_h(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 34

       if ( dtr(l,k)%var_a(i,j) /= i+j+(j-1)*DIM2 ) error stop 36
       if ( dtr(l,k)%var_b(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 38
       if ( dtr(l,k)%var_c(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 40
       if ( dtr(l,k)%var_d(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 42
       if ( dtr(l,k)%var_e(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 44
       if ( dtr(l,k)%var_f(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 46
       if ( dtr(l,k)%var_g(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 48
       if ( dtr(l,k)%var_h(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 50

       if ( dta(l,k)%vdt2(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2 ) error stop 52
       if ( dta(l,k)%vdt2(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 54
       if ( dta(l,k)%vdt2(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 56
       if ( dta(l,k)%vdt2(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 58
       if ( dta(l,k)%vdt2(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 60
       if ( dta(l,k)%vdt2(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 62
       if ( dta(l,k)%vdt2(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 64
       if ( dta(l,k)%vdt2(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 66

       if ( dtr(l,k)%vdt2(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2 ) error stop 68
       if ( dtr(l,k)%vdt2(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 70
       if ( dtr(l,k)%vdt2(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 72
       if ( dtr(l,k)%vdt2(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 74
       if ( dtr(l,k)%vdt2(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 76
       if ( dtr(l,k)%vdt2(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 78
       if ( dtr(l,k)%vdt2(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 80
       if ( dtr(l,k)%vdt2(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 82

       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2 ) error stop 84
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 86
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 88
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 90
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 92
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 94
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 96
       if ( dta(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 98

       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2 ) error stop 100
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 102
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 104
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 106
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 108
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 110
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 112
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+7 ) error stop 114
      end do
     end do
    end do
   end do

!! Test 3

   dta = reshape((/dt0,dt0,dt0,dt0,dt0,dt0/),(/DIM2,DIM1/))

   dtp = fun3(dta+dta,dtb)

   call C_F_POINTER(dtp,dtr,(/DIM2,DIM1/))

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtb(l,k)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1)+1 ) error stop 116
       if ( dtb(l,k)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 118
       if ( dtb(l,k)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+2)+1 ) error stop 120
       if ( dtb(l,k)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+3)+2 ) error stop 122
       if ( dtb(l,k)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+4)+3 ) error stop 124
       if ( dtb(l,k)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+5)+4 ) error stop 126
       if ( dtb(l,k)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+6)+5 ) error stop 128
       if ( dtb(l,k)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+7)+6 ) error stop 130

       if ( dtr(l,k)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1)+1 ) error stop 132
       if ( dtr(l,k)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 134
       if ( dtr(l,k)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+2)+1 ) error stop 136
       if ( dtr(l,k)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+3)+2 ) error stop 138
       if ( dtr(l,k)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+4)+3 ) error stop 140
       if ( dtr(l,k)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+5)+4 ) error stop 142
       if ( dtr(l,k)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+6)+5 ) error stop 144
       if ( dtr(l,k)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+7)+6 ) error stop 146

       if ( dtb(l,k)%vdt2(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1)+1 ) error stop 148
       if ( dtb(l,k)%vdt2(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 150
       if ( dtb(l,k)%vdt2(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+2)+1 ) error stop 152
       if ( dtb(l,k)%vdt2(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+3)+2 ) error stop 154
       if ( dtb(l,k)%vdt2(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+4)+3 ) error stop 156
       if ( dtb(l,k)%vdt2(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+5)+4 ) error stop 158
       if ( dtb(l,k)%vdt2(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+6)+5 ) error stop 160
       if ( dtb(l,k)%vdt2(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+7)+6 ) error stop 162

       if ( dtr(l,k)%vdt2(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1)+1 ) error stop 164
       if ( dtr(l,k)%vdt2(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 166
       if ( dtr(l,k)%vdt2(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+2)+1 ) error stop 168
       if ( dtr(l,k)%vdt2(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+3)+2 ) error stop 170
       if ( dtr(l,k)%vdt2(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+4)+3 ) error stop 172
       if ( dtr(l,k)%vdt2(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+5)+4 ) error stop 174
       if ( dtr(l,k)%vdt2(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+6)+5 ) error stop 176
       if ( dtr(l,k)%vdt2(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+7)+6 ) error stop 178

       if ( dtb(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1)+1 ) error stop 180
       if ( dtb(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 182
       if ( dtb(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+2)+1 ) error stop 184
       if ( dtb(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+3)+2 ) error stop 186
       if ( dtb(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+4)+3 ) error stop 188
       if ( dtb(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+5)+4 ) error stop 190
       if ( dtb(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+6)+5 ) error stop 192
       if ( dtb(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+7)+6 ) error stop 194

       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1)+1 ) error stop 196
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 198
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+2)+1 ) error stop 200
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+3)+2 ) error stop 202
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+4)+3 ) error stop 204
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+5)+4 ) error stop 206
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+6)+5 ) error stop 208
       if ( dtr(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+7)+6 ) error stop 210
      end do
     end do
    end do
   end do

end program fxdtyia45a

function fun1(dtk) bind(c)
   use ISO_C_BINDING

   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(1) var_a(DIM1,DIM2)
      integer(C_INT) var_b(DIM1,DIM2)
      integer(C_SHORT) var_c(DIM1,DIM2)
      integer(8) var_d(DIM1,DIM2)
      integer(C_LONG) var_e(DIM1,DIM2)
      integer(2) var_f(DIM1,DIM2)
      integer(C_LONG_LONG) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt2
      integer(C_INT16_T) var_a(DIM1,DIM2)
      integer(C_INT_FAST8_T) var_b(DIM1,DIM2)
      integer(C_INT64_T) var_c(DIM1,DIM2)
      integer(C_INT8_T) var_d(DIM1,DIM2)
      integer(C_INT_FAST32_T) var_e(DIM1,DIM2)
      integer(C_INT_FAST16_T) var_f(DIM1,DIM2)
      integer(C_INT32_T) var_g(DIM1,DIM2)
      integer(C_INT_FAST64_T) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2(DIM2,DIM1)
      integer(C_SIZE_T) var_a(DIM1,DIM2)
      integer(C_INT_LEAST8_T) var_b(DIM1,DIM2)
      integer(C_INT_LEAST64_T) var_c(DIM1,DIM2)
      integer(C_SIGNED_CHAR) var_d(DIM1,DIM2)
      integer(C_INTPTR_T) var_e(DIM1,DIM2)
      integer(C_INT_LEAST16_T) var_f(DIM1,DIM2)
      integer(C_INTMAX_T) var_g(DIM1,DIM2)
      integer(C_INT_LEAST32_T) var_h(DIM1,DIM2)
   end type

   type(dt3), dimension(DIM2,DIM1), target :: dtk
   type(C_PTR) :: fun1

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtk(l,k)%var_a(i,j) /= i+j+(j-1)*DIM2-1 ) error stop 212
       if ( dtk(l,k)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 214
       if ( dtk(l,k)%var_c(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 216
       if ( dtk(l,k)%var_d(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 218
       if ( dtk(l,k)%var_e(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 220
       if ( dtk(l,k)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 222
       if ( dtk(l,k)%var_g(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 224
       if ( dtk(l,k)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 226

       if ( dtk(l,k)%vdt2(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2-1 ) error stop 228
       if ( dtk(l,k)%vdt2(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 230
       if ( dtk(l,k)%vdt2(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 232
       if ( dtk(l,k)%vdt2(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 234
       if ( dtk(l,k)%vdt2(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 236
       if ( dtk(l,k)%vdt2(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 238
       if ( dtk(l,k)%vdt2(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 240
       if ( dtk(l,k)%vdt2(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 242

       if ( dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) /= i+j+(j-1)*DIM2-1 ) error stop 244
       if ( dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= i+j+(j-1)*DIM2 ) error stop 246
       if ( dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= i+j+(j-1)*DIM2+1 ) error stop 248
       if ( dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= i+j+(j-1)*DIM2+2 ) error stop 250
       if ( dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= i+j+(j-1)*DIM2+3 ) error stop 252
       if ( dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= i+j+(j-1)*DIM2+4 ) error stop 254
       if ( dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) /= i+j+(j-1)*DIM2+5 ) error stop 256
       if ( dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= i+j+(j-1)*DIM2+6 ) error stop 258
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dtk(l,k)%var_a(i,j) = i+j+(j-1)*DIM2
       dtk(l,k)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dtk(l,k)%var_c(i,j) = i+j+(j-1)*DIM2+2
       dtk(l,k)%var_d(i,j) = i+j+(j-1)*DIM2+3
       dtk(l,k)%var_e(i,j) = i+j+(j-1)*DIM2+4
       dtk(l,k)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dtk(l,k)%var_g(i,j) = i+j+(j-1)*DIM2+6
       dtk(l,k)%var_h(i,j) = i+j+(j-1)*DIM2+7

       dtk(l,k)%vdt2(j,i)%var_a(i,j) = i+j+(j-1)*DIM2
       dtk(l,k)%vdt2(j,i)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dtk(l,k)%vdt2(j,i)%var_c(i,j) = i+j+(j-1)*DIM2+2
       dtk(l,k)%vdt2(j,i)%var_d(i,j) = i+j+(j-1)*DIM2+3
       dtk(l,k)%vdt2(j,i)%var_e(i,j) = i+j+(j-1)*DIM2+4
       dtk(l,k)%vdt2(j,i)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dtk(l,k)%vdt2(j,i)%var_g(i,j) = i+j+(j-1)*DIM2+6
       dtk(l,k)%vdt2(j,i)%var_h(i,j) = i+j+(j-1)*DIM2+7

       dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) = i+j+(j-1)*DIM2
       dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) = i+j+(j-1)*DIM2+1
       dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) = i+j+(j-1)*DIM2+2
       dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) = i+j+(j-1)*DIM2+3
       dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) = i+j+(j-1)*DIM2+4
       dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) = i+j+(j-1)*DIM2+5
       dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) = i+j+(j-1)*DIM2+6
       dtk(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) = i+j+(j-1)*DIM2+7
      end do
     end do
    end do
   end do

  fun1 = C_LOC(dtk)
end function fun1

function fun3(dtu,dty) bind(c)
   use ISO_C_BINDING

   integer, parameter :: DIM1 = 3, DIM2 = 2

   type, bind(c) :: dt1
      integer(1) var_a(DIM1,DIM2)
      integer(C_INT) var_b(DIM1,DIM2)
      integer(C_SHORT) var_c(DIM1,DIM2)
      integer(8) var_d(DIM1,DIM2)
      integer(C_LONG) var_e(DIM1,DIM2)
      integer(2) var_f(DIM1,DIM2)
      integer(C_LONG_LONG) var_g(DIM1,DIM2)
      integer(4) var_h(DIM1,DIM2)
   end type

   type, bind(c) :: dt2
      integer(C_INT16_T) var_a(DIM1,DIM2)
      integer(C_INT_FAST8_T) var_b(DIM1,DIM2)
      integer(C_INT64_T) var_c(DIM1,DIM2)
      integer(C_INT8_T) var_d(DIM1,DIM2)
      integer(C_INT_FAST32_T) var_e(DIM1,DIM2)
      integer(C_INT_FAST16_T) var_f(DIM1,DIM2)
      integer(C_INT32_T) var_g(DIM1,DIM2)
      integer(C_INT_FAST64_T) var_h(DIM1,DIM2)
      type(dt1) :: vdt1(DIM2,DIM1)
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2(DIM2,DIM1)
      integer(C_SIZE_T) var_a(DIM1,DIM2)
      integer(C_INT_LEAST8_T) var_b(DIM1,DIM2)
      integer(C_INT_LEAST64_T) var_c(DIM1,DIM2)
      integer(C_SIGNED_CHAR) var_d(DIM1,DIM2)
      integer(C_INTPTR_T) var_e(DIM1,DIM2)
      integer(C_INT_LEAST16_T) var_f(DIM1,DIM2)
      integer(C_INTMAX_T) var_g(DIM1,DIM2)
      integer(C_INT_LEAST32_T) var_h(DIM1,DIM2)
   end type

   type(dt3), dimension(DIM2,DIM1), intent(in) :: dtu
   type(dt3), dimension(DIM2,DIM1), intent(out), target :: dty
   type(C_PTR) :: fun3

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       if ( dtu(l,k)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1) ) error stop 260
       if ( dtu(l,k)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 262
       if ( dtu(l,k)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 264
       if ( dtu(l,k)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 266
       if ( dtu(l,k)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 268
       if ( dtu(l,k)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 270
       if ( dtu(l,k)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 272
       if ( dtu(l,k)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 274

       if ( dtu(l,k)%vdt2(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1) ) error stop 276
       if ( dtu(l,k)%vdt2(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 278
       if ( dtu(l,k)%vdt2(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 280
       if ( dtu(l,k)%vdt2(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 282
       if ( dtu(l,k)%vdt2(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 284
       if ( dtu(l,k)%vdt2(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 286
       if ( dtu(l,k)%vdt2(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 288
       if ( dtu(l,k)%vdt2(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 290

       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) /= 2*(i+j+(j-1)*DIM2-1) ) error stop 292
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) /= 2*(i+j+(j-1)*DIM2) ) error stop 294
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) /= 2*(i+j+(j-1)*DIM2+1) ) error stop 296
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) /= 2*(i+j+(j-1)*DIM2+2) ) error stop 298
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) /= 2*(i+j+(j-1)*DIM2+3) ) error stop 300
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) /= 2*(i+j+(j-1)*DIM2+4) ) error stop 302
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) /= 2*(i+j+(j-1)*DIM2+5) ) error stop 304
       if ( dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) /= 2*(i+j+(j-1)*DIM2+6) ) error stop 306
      end do
     end do
    end do
   end do

   do k = 1, DIM1
    do l = 1, DIM2
     do j = 1, DIM2
      do i = 1, DIM1
       dty(l,k)%var_a(i,j) = dtu(l,k)%var_a(i,j) + 1
       dty(l,k)%var_b(i,j) = dtu(l,k)%var_b(i,j) + 2
       dty(l,k)%var_c(i,j) = dtu(l,k)%var_c(i,j) + 3
       dty(l,k)%var_d(i,j) = dtu(l,k)%var_d(i,j) + 4
       dty(l,k)%var_e(i,j) = dtu(l,k)%var_e(i,j) + 5
       dty(l,k)%var_f(i,j) = dtu(l,k)%var_f(i,j) + 6
       dty(l,k)%var_g(i,j) = dtu(l,k)%var_g(i,j) + 7
       dty(l,k)%var_h(i,j) = dtu(l,k)%var_h(i,j) + 8

       dty(l,k)%vdt2(j,i)%var_a(i,j) = dtu(l,k)%vdt2(j,i)%var_a(i,j) + 1
       dty(l,k)%vdt2(j,i)%var_b(i,j) = dtu(l,k)%vdt2(j,i)%var_b(i,j) + 2
       dty(l,k)%vdt2(j,i)%var_c(i,j) = dtu(l,k)%vdt2(j,i)%var_c(i,j) + 3
       dty(l,k)%vdt2(j,i)%var_d(i,j) = dtu(l,k)%vdt2(j,i)%var_d(i,j) + 4
       dty(l,k)%vdt2(j,i)%var_e(i,j) = dtu(l,k)%vdt2(j,i)%var_e(i,j) + 5
       dty(l,k)%vdt2(j,i)%var_f(i,j) = dtu(l,k)%vdt2(j,i)%var_f(i,j) + 6
       dty(l,k)%vdt2(j,i)%var_g(i,j) = dtu(l,k)%vdt2(j,i)%var_g(i,j) + 7
       dty(l,k)%vdt2(j,i)%var_h(i,j) = dtu(l,k)%vdt2(j,i)%var_h(i,j) + 8

       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) = dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_a(i,j) + 1
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) = dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_b(i,j) + 2
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) = dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_c(i,j) + 3
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) = dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_d(i,j) + 4
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) = dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_e(i,j) + 5
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) = dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_f(i,j) + 6
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) = dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_g(i,j) + 7
       dty(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) = dtu(l,k)%vdt2(j,i)%vdt1(j,i)%var_h(i,j) + 8
      end do
     end do
    end do
   end do

   fun3 = C_LOC(dty)
end function fun3