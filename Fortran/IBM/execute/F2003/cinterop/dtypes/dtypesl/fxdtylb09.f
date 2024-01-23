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
!*      - Testing 2-levels deep derived types with BIND(C) attribute
!*      - Testing 2-levels deep derived types with VALUE, INTENT attributes
!*      - Testing 2-levels deep derived types with logical, character and real components
!*      - Testing FORTRAN functions and C functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtylb09
   use ISO_C_BINDING

   type, bind(c) :: dt1
      logical(C_BOOL) var_a
      real(C_DOUBLE) var_b
      character(C_CHAR) var_c
      real(C_FLOAT) var_d
      real(16) var_e
      real(8) var_f
      logical(C_BOOL) var_g
      real(4) var_h
   end type

   type, bind(c) :: dt2
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(16) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
      type(dt1) :: vdt1
   end type

end module mxdtylb09

module auxmod
   use mxdtylb09

   interface operator(+)
      module procedure adddty_s, adddty_d
   end interface

!----------------------------------------------------------
contains
   function adddty_s(dtx,dty)
      type(dt1), intent(in) :: dtx, dty
      type(dt1) :: adddty_s

      adddty_s%var_a = dtx%var_a .and. dty%var_a
      adddty_s%var_b = dtx%var_b + dty%var_b
      adddty_s%var_c = dtx%var_c
      adddty_s%var_d = dtx%var_d + dty%var_d
      adddty_s%var_e = dtx%var_e + dty%var_e
      adddty_s%var_f = dtx%var_f + dty%var_f
      adddty_s%var_g = dtx%var_g .and. dty%var_g
      adddty_s%var_h = dtx%var_h + dty%var_h

   end function adddty_s

   function adddty_d(dtx,dty)
      type(dt2), intent(in) :: dtx, dty
      type(dt2) :: adddty_d

      adddty_d%var_a = dtx%var_a + dty%var_a
      adddty_d%var_b = dtx%var_b .and. dty%var_b
      adddty_d%var_c = dtx%var_c + dty%var_c
      adddty_d%var_d = dtx%var_d + dty%var_d
      adddty_d%var_e = dtx%var_e + dty%var_e
      adddty_d%var_f = dtx%var_f .and. dty%var_f
      adddty_d%var_g = dtx%var_g + dty%var_g
      adddty_d%var_h = dtx%var_h

      adddty_d%vdt1 = adddty_s(dtx%vdt1,dty%vdt1)

   end function adddty_d

end module auxmod

program fxdtylb09
   use mxdtylb09
   use auxmod
   interface
      function fun1(dt) bind(c)
         import dt2
         type(dt2) :: fun1, dt
      end function fun1
      function fun2(dt) bind(c)
         import dt2
         type(dt2), value :: dt
         type(dt2) :: fun2
      end function fun2
      function fun3(dtx,dty) bind(c)
         import dt2
         type(dt2), intent(in) :: dtx
         type(dt2), intent(out) :: dty
         type(dt2) :: fun3
      end function fun3
      function fun4(dtx,dty) bind(c)
         import dt2, C_PTR
         type(dt2), intent(in) :: dtx
         type(dt2), intent(out) :: dty
         type(C_PTR) :: fun4
      end function fun4
   end interface

   type(dt2) :: dt0 = dt2(2.0e0,.true.,6.0q0,8.0d0,10.0e0,.true.,14.0d0,'A', &
                      dt1(.true.,4.0d0,'A',8.0e0,10.0q0,12.0d0,.true.,16.0e0))

   type(dt2) :: dta, dtb, dtc
   type(dt2), pointer :: dtd
   type(C_PTR) :: dtp

!----------------------------------------------------------
!! Test 1

   dta = dt0

   dtb = fun1(dta)

   if ( dta%var_a /= 3.0e0 .or. dtb%var_a /= 3.0e0 ) error stop 20
   if ( (dta%var_b .neqv. .false.) .or. (dtb%var_b .neqv. .false.) ) error stop 22
   if ( dta%var_c /= 7.0q0 .or. dtb%var_c /= 7.0q0 ) error stop 24
   if ( dta%var_d /= 9.0d0 .or. dtb%var_d /= 9.0d0 ) error stop 26
   if ( dta%var_e /= 11.0e0 .or. dtb%var_e /= 11.0e0 ) error stop 28
   if ( (dta%var_f .neqv. .false.) .or. (dtb%var_f .neqv. .false.) ) error stop 30
   if ( dta%var_g /= 15.0d0 .or. dtb%var_g /= 15.0d0 ) error stop 32
   if ( (dta%var_h /= 'B') .or. (dtb%var_h /= 'B') ) error stop 34

   if ( (dta%vdt1%var_a .neqv. .false.) .or. (dtb%vdt1%var_a .neqv. .false.) ) error stop 36
   if ( dta%vdt1%var_b /= 6 .or. dtb%vdt1%var_b /= 6 ) error stop 38
   if ( (dta%vdt1%var_c /= 'B') .or. (dtb%vdt1%var_c /= 'B') ) error stop 40
   if ( dta%vdt1%var_d /= 10 .or. dtb%vdt1%var_d /= 10 ) error stop 42
   if ( dta%vdt1%var_e /= 12 .or. dtb%vdt1%var_e /= 12 ) error stop 44
   if ( dta%vdt1%var_f /= 14 .or. dtb%vdt1%var_f /= 14 ) error stop 46
   if ( (dta%vdt1%var_g .neqv. .false.) .or. (dtb%vdt1%var_g .neqv. .false.) ) error stop 48
   if ( dta%vdt1%var_h /= 18 .or. dtb%vdt1%var_h /= 18 ) error stop 50

!----------------------------------------------------------
!! Test 2

   dta = dt0

   dtb = fun2(dta)

   if ( dta%var_a /= 2.0e0 .or. dtb%var_a /= 3.0e0 ) error stop 52
   if ( (dta%var_b .neqv. .true.) .or. (dtb%var_b .neqv. .false.) ) error stop 54
   if ( dta%var_c /= 6.0q0 .or. dtb%var_c /= 7.0q0 ) error stop 56
   if ( dta%var_d /= 8.0d0 .or. dtb%var_d /= 9.0d0 ) error stop 58
   if ( dta%var_e /= 10.0e0 .or. dtb%var_e /= 11.0e0 ) error stop 60
   if ( (dta%var_f .neqv. .true.) .or. (dtb%var_f .neqv. .false.) ) error stop 62
   if ( dta%var_g /= 14.0d0 .or. dtb%var_g /= 15.0d0 ) error stop 64
   if ( (dta%var_h /= 'A') .or. (dtb%var_h /= 'B') ) error stop 66

   if ( (dta%vdt1%var_a .neqv. .true.) .or. (dtb%vdt1%var_a .neqv. .false.) ) error stop 68
   if ( dta%vdt1%var_b /= 4 .or. dtb%vdt1%var_b /= 6 ) error stop 70
   if ( (dta%vdt1%var_c /= 'A') .or. (dtb%vdt1%var_c /= 'B') ) error stop 72
   if ( dta%vdt1%var_d /= 8 .or. dtb%vdt1%var_d /= 10 ) error stop 74
   if ( dta%vdt1%var_e /= 10 .or. dtb%vdt1%var_e /= 12 ) error stop 76
   if ( dta%vdt1%var_f /= 12 .or. dtb%vdt1%var_f /= 14 ) error stop 78
   if ( (dta%vdt1%var_g .neqv. .true.) .or. (dtb%vdt1%var_g .neqv. .false.) ) error stop 80
   if ( dta%vdt1%var_h /= 16 .or. dtb%vdt1%var_h /= 18 ) error stop 82

!----------------------------------------------------------
!! Test 3

   dta = dt0

   dtc = fun3(dta+dta,dtb)

   if ( dtb%var_a /= 5.0e0 .or. dtc%var_a /= 5.0e0 ) error stop 84
   if ( (dtb%var_b .neqv. .false.) .or. (dtc%var_b .neqv. .false.) ) error stop 86
   if ( dtb%var_c /= 13.0q0 .or. dtc%var_c /= 13.0q0 ) error stop 88
   if ( dtb%var_d /= 17.0d0 .or. dtc%var_d /= 17.0d0 ) error stop 90
   if ( dtb%var_e /= 21.0e0 .or. dtc%var_e /= 21.0e0 ) error stop 92
   if ( (dtb%var_f .neqv. .false.) .or. (dtc%var_f .neqv. .false.) ) error stop 94
   if ( dtb%var_g /= 29.0d0 .or. dtc%var_g /= 29.0d0 ) error stop 96
   if ( (dtb%var_h /= 'B') .or. (dtc%var_h /= 'B') ) error stop 98

   if ( (dtb%vdt1%var_a .neqv. .false.) .or. (dtc%vdt1%var_a .neqv. .false.) ) error stop 100
   if ( dtb%vdt1%var_b /= 10 .or. dtc%vdt1%var_b /= 10 ) error stop 102
   if ( (dtb%vdt1%var_c /= 'B') .or. (dtc%vdt1%var_c /= 'B') ) error stop 104
   if ( dtb%vdt1%var_d /= 18 .or. dtc%vdt1%var_d /= 18 ) error stop 106
   if ( dtb%vdt1%var_e /= 22 .or. dtc%vdt1%var_e /= 22 ) error stop 108
   if ( dtb%vdt1%var_f /= 26 .or. dtc%vdt1%var_f /= 26 ) error stop 110
   if ( (dtb%vdt1%var_g .neqv. .false.) .or. (dtc%vdt1%var_g .neqv. .false.) ) error stop 112
   if ( dtb%vdt1%var_h /= 34 .or. dtc%vdt1%var_h /= 34 ) error stop 114

!----------------------------------------------------------
!! Test 4

   dta = dt0

   dtp = fun4(dta+dta,dtb)

   call C_F_POINTER(dtp,dtd)

   if ( dtb%var_a /= 5.0e0 .or. dtd%var_a /= 5.0e0 ) error stop 116
   if ( (dtb%var_b .neqv. .false.) .or. (dtd%var_b .neqv. .false.) ) error stop 118
   if ( dtb%var_c /= 13.0q0 .or. dtd%var_c /= 13.0q0 ) error stop 120
   if ( dtb%var_d /= 17.0d0 .or. dtd%var_d /= 17.0d0 ) error stop 122
   if ( dtb%var_e /= 21.0e0 .or. dtd%var_e /= 21.0e0 ) error stop 124
   if ( (dtb%var_f .neqv. .false.) .or. (dtd%var_f .neqv. .false.) ) error stop 126
   if ( dtb%var_g /= 29.0d0 .or. dtd%var_g /= 29.0d0 ) error stop 128
   if ( (dtb%var_h /= 'B') .or. (dtd%var_h /= 'B') ) error stop 130

   if ( (dtb%vdt1%var_a .neqv. .false.) .or. (dtd%vdt1%var_a .neqv. .false.) ) error stop 132
   if ( dtb%vdt1%var_b /= 10 .or. dtd%vdt1%var_b /= 10 ) error stop 134
   if ( (dtb%vdt1%var_c /= 'B') .or. (dtd%vdt1%var_c /= 'B') ) error stop 136
   if ( dtb%vdt1%var_d /= 18 .or. dtd%vdt1%var_d /= 18 ) error stop 138
   if ( dtb%vdt1%var_e /= 22 .or. dtd%vdt1%var_e /= 22 ) error stop 140
   if ( dtb%vdt1%var_f /= 26 .or. dtd%vdt1%var_f /= 26 ) error stop 142
   if ( (dtb%vdt1%var_g .neqv. .false.) .or. (dtd%vdt1%var_g .neqv. .false.) ) error stop 144
   if ( dtb%vdt1%var_h /= 34 .or. dtd%vdt1%var_h /= 34 ) error stop 146

!----------------------------------------------------------
end program fxdtylb09
