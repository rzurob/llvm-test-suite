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
!*      - Testing 3-levels deep derived types with BIND(C) attribute
!*      - Testing 3-levels deep derived types with VALUE, INTENT attributes
!*      - Testing 3-levels deep derived types with logical, character and real components
!*      - Testing FORTRAN external functions
!*      - Main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mxdtylb39
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
      real(C_DOUBLE) var_a
      logical(C_BOOL) var_b
      real(8) var_c
      character(C_CHAR) var_d
      real(16) var_e
      real(4) var_f
      logical(C_BOOL) var_g
      real(C_FLOAT) var_h
      type(dt1) :: vdt1
   end type

   type, bind(c) :: dt3
      type(dt2) :: vdt2
      real(C_FLOAT) var_a
      logical(C_BOOL) var_b
      real(16) var_c
      real(8) var_d
      real(4) var_e
      logical(C_BOOL) var_f
      real(C_DOUBLE) var_g
      character(C_CHAR) var_h
   end type

end module mxdtylb39

module auxmod
   use mxdtylb39

   interface operator(+)
      module procedure adddty_s, adddty_d, adddty_t
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
      adddty_d%var_d = dtx%var_d
      adddty_d%var_e = dtx%var_e + dty%var_e
      adddty_d%var_f = dtx%var_f + dty%var_f
      adddty_d%var_g = dtx%var_g .and. dty%var_g
      adddty_d%var_h = dtx%var_h + dty%var_h

      adddty_d%vdt1 = adddty_s(dtx%vdt1,dty%vdt1)

   end function adddty_d

   function adddty_t(dtx,dty)
      type(dt3), intent(in) :: dtx, dty
      type(dt3) :: adddty_t

      adddty_t%var_a = dtx%var_a + dty%var_a
      adddty_t%var_b = dtx%var_b .and. dty%var_b
      adddty_t%var_c = dtx%var_c + dty%var_c
      adddty_t%var_d = dtx%var_d + dty%var_d
      adddty_t%var_e = dtx%var_e + dty%var_e
      adddty_t%var_f = dtx%var_f .and. dty%var_f
      adddty_t%var_g = dtx%var_g + dty%var_g
      adddty_t%var_h = dtx%var_h

      adddty_t%vdt2 = adddty_d(dtx%vdt2,dty%vdt2)

   end function adddty_t

end module auxmod

program fxdtylb39
   use mxdtylb39
   use auxmod
   interface
      function fun1(dt) bind(c)
         import dt3
         type(dt3) :: fun1, dt
      end function fun1
      function fun2(dt) bind(c)
         import dt3
         type(dt3), value :: dt
         type(dt3) :: fun2
      end function fun2
      function fun3(dtx,dty) bind(c)
         import dt3
         type(dt3), intent(in) :: dtx
         type(dt3), intent(out) :: dty
         type(dt3) :: fun3
      end function fun3
      function fun4(dtx,dty) bind(c)
         import dt3, C_PTR
         type(dt3), intent(in) :: dtx
         type(dt3), intent(out) :: dty
         type(C_PTR) :: fun4
      end function fun4
   end interface

   type(dt3) :: dt0 = dt3(dt2(2.0d0,.true.,6.0d0,'A',10.0q0,12.0e0,.true.,16.0e0, &
                      dt1(.true.,4.0d0,'A',8.0e0,10.0q0,12.0d0,.true.,16.0e0)), &
                      2.0e0,.true.,6.0q0,8.0d0,10.0e0,.true.,14.0d0,'A')

   type(dt3) :: dta, dtb, dtc
   type(dt3), pointer :: dtd
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

   if ( dta%vdt2%var_a /= 4.0d0 .or. dtb%vdt2%var_a /= 4.0d0 ) error stop 36
   if ( (dta%vdt2%var_b .neqv. .false.) .or. (dtb%vdt2%var_b .neqv. .false.) ) error stop 38
   if ( dta%vdt2%var_c /= 8.0d0 .or. dtb%vdt2%var_c /= 8.0d0 ) error stop 40
   if ( (dta%vdt2%var_d /= 'B') .or. (dtb%vdt2%var_d /= 'B') ) error stop 42
   if ( dta%vdt2%var_e /= 12.0q0 .or. dtb%vdt2%var_e /= 12.0q0 ) error stop 44
   if ( dta%vdt2%var_f /= 14.0e0 .or. dtb%vdt2%var_f /= 14.0e0 ) error stop 46
   if ( (dta%vdt2%var_g .neqv. .false.) .or. (dtb%vdt2%var_g .neqv. .false.) ) error stop 48
   if ( dta%vdt2%var_h /= 18.0e0 .or. dtb%vdt2%var_h /= 18.0e0 ) error stop 50

   if ( (dta%vdt2%vdt1%var_a .neqv. .false.) .or. &
                               (dtb%vdt2%vdt1%var_a .neqv. .false.) ) error stop 52
   if ( dta%vdt2%vdt1%var_b /= 7.0d0 .or. &
                               dta%vdt2%vdt1%var_b /= 7.0d0 ) error stop 54
   if ( (dta%vdt2%vdt1%var_c /= 'B') .or. &
                               (dta%vdt2%vdt1%var_c /= 'B') ) error stop 56
   if ( dta%vdt2%vdt1%var_d /= 11.0e0 .or. &
                               dta%vdt2%vdt1%var_d /= 11.0e0 ) error stop 58
   if ( dta%vdt2%vdt1%var_e /= 13.0q0 .or. &
                               dta%vdt2%vdt1%var_e /= 13.0q0 ) error stop 60
   if ( dta%vdt2%vdt1%var_f /= 15.0d0 .or. &
                               dta%vdt2%vdt1%var_f /= 15.0d0 ) error stop 62
   if ( (dta%vdt2%vdt1%var_g .neqv. .false.) .or. &
                               (dta%vdt2%vdt1%var_g .neqv. .false.) ) error stop 64
   if ( dta%vdt2%vdt1%var_h /= 19.0e0 .or. &
                               dta%vdt2%vdt1%var_h /= 19.0e0 ) error stop 66

!----------------------------------------------------------
!! Test 2

   dta = dt0

   dtb = fun2(dta)

   if ( dta%var_a /= 2.0e0 .or. dtb%var_a /= 3.0e0 ) error stop 68
   if ( (dta%var_b .neqv. .true.) .or. (dtb%var_b .neqv. .false.) ) error stop 70
   if ( dta%var_c /= 6.0q0 .or. dtb%var_c /= 7.0q0 ) error stop 72
   if ( dta%var_d /= 8.0d0 .or. dtb%var_d /= 9.0d0 ) error stop 74
   if ( dta%var_e /= 10.0e0 .or. dtb%var_e /= 11.0e0 ) error stop 76
   if ( (dta%var_f .neqv. .true.) .or. (dtb%var_f .neqv. .false.) ) error stop 78
   if ( dta%var_g /= 14.0d0 .or. dtb%var_g /= 15.0d0 ) error stop 80
   if ( (dta%var_h /= 'A') .or. (dtb%var_h /= 'B') ) error stop 82

   if ( dta%vdt2%var_a /= 2.0d0 .or. dtb%vdt2%var_a /= 4.0d0 ) error stop 84
   if ( (dta%vdt2%var_b .neqv. .true.) .or. (dtb%vdt2%var_b .neqv. .false.) ) error stop 86
   if ( dta%vdt2%var_c /= 6.0d0 .or. dtb%vdt2%var_c /= 8.0d0 ) error stop 88
   if ( (dta%vdt2%var_d /= 'A') .or. (dtb%vdt2%var_d /= 'B') ) error stop 90
   if ( dta%vdt2%var_e /= 10.0q0 .or. dtb%vdt2%var_e /= 12.0q0 ) error stop 92
   if ( dta%vdt2%var_f /= 12.0e0 .or. dtb%vdt2%var_f /= 14.0e0 ) error stop 94
   if ( (dta%vdt2%var_g .neqv. .true.) .or. (dtb%vdt2%var_g .neqv. .false.) ) error stop 96
   if ( dta%vdt2%var_h /= 16.0e0 .or. dtb%vdt2%var_h /= 18.0e0 ) error stop 98

   if ( (dta%vdt2%vdt1%var_a .neqv. .true.) .or. &
                               (dtb%vdt2%vdt1%var_a .neqv. .false.) ) error stop 100
   if ( dta%vdt2%vdt1%var_b /= 4.0d0 .or. &
                               dtb%vdt2%vdt1%var_b /= 7.0d0 ) error stop 102
   if ( (dta%vdt2%vdt1%var_c /= 'A') .or. &
                               (dtb%vdt2%vdt1%var_c /= 'B') ) error stop 104
   if ( dta%vdt2%vdt1%var_d /= 8.0e0 .or. &
                               dtb%vdt2%vdt1%var_d /= 11.0e0 ) error stop 106
   if ( dta%vdt2%vdt1%var_e /= 10.0q0 .or. &
                               dtb%vdt2%vdt1%var_e /= 13.0q0 ) error stop 108
   if ( dta%vdt2%vdt1%var_f /= 12.0d0 .or. &
                               dtb%vdt2%vdt1%var_f /= 15.0d0 ) error stop 110
   if ( (dta%vdt2%vdt1%var_g .neqv. .true.) .or. &
                               (dtb%vdt2%vdt1%var_g .neqv. .false.) ) error stop 112
   if ( dta%vdt2%vdt1%var_h /= 16.0e0 .or. &
                               dtb%vdt2%vdt1%var_h /= 19.0e0 ) error stop 114

!----------------------------------------------------------
!! Test 3

   dta = dt0

   dtc = fun3(dta+dta,dtb)

   if ( dtb%var_a /= 5.0e0 .or. dtc%var_a /= 5.0e0 ) error stop 116
   if ( (dtb%var_b .neqv. .false.) .or. (dtc%var_b .neqv. .false.) ) error stop 118
   if ( dtb%var_c /= 13.0q0 .or. dtc%var_c /= 13.0q0 ) error stop 120
   if ( dtb%var_d /= 17.0d0 .or. dtc%var_d /= 17.0d0 ) error stop 122
   if ( dtb%var_e /= 21.0e0 .or. dtc%var_e /= 21.0e0 ) error stop 124
   if ( (dtb%var_f .neqv. .false.) .or. (dtc%var_f .neqv. .false.) ) error stop 126
   if ( dtb%var_g /= 29.0d0 .or. dtc%var_g /= 29.0d0 ) error stop 128
   if ( (dtb%var_h /= 'B') .or. (dtc%var_h /= 'B') ) error stop 130

   if ( dtb%vdt2%var_a /= 6.0d0 .or. dtc%vdt2%var_a /= 6.0d0 ) error stop 132
   if ( (dtb%vdt2%var_b .neqv. .false.) .or. (dtc%vdt2%var_b .neqv. .false.) ) error stop 134
   if ( dtb%vdt2%var_c /= 14.0d0 .or. dtc%vdt2%var_c /= 14.0d0 ) error stop 136
   if ( (dtb%vdt2%var_d /= 'B') .or. (dtc%vdt2%var_d /= 'B') ) error stop 138
   if ( dtb%vdt2%var_e /= 22.0q0 .or. dtc%vdt2%var_e /= 22.0q0 ) error stop 140
   if ( dtb%vdt2%var_f /= 26.0e0 .or. dtc%vdt2%var_f /= 26.0e0 ) error stop 142
   if ( (dtb%vdt2%var_g .neqv. .false.) .or. (dtc%vdt2%var_g .neqv. .false.) ) error stop 144
   if ( dtb%vdt2%var_h /= 34.0e0 .or. dtc%vdt2%var_h /= 34.0e0 ) error stop 146

   if ( (dtb%vdt2%vdt1%var_a .neqv. .false.) .or. &
                              (dtc%vdt2%vdt1%var_a .neqv. .false.) ) error stop 148
   if ( dtb%vdt2%vdt1%var_b /= 11.0d0 .or. &
                              dtc%vdt2%vdt1%var_b /= 11.0d0 ) error stop 150
   if ( (dtb%vdt2%vdt1%var_c /= 'B') .or. &
                              (dtc%vdt2%vdt1%var_c /= 'B') ) error stop 152
   if ( dtb%vdt2%vdt1%var_d /= 19.0e0 .or. &
                              dtc%vdt2%vdt1%var_d /= 19.0e0 ) error stop 154
   if ( dtb%vdt2%vdt1%var_e /= 23.0q0 .or. &
                              dtc%vdt2%vdt1%var_e /= 23.0q0 ) error stop 156
   if ( dtb%vdt2%vdt1%var_f /= 27.0d0 .or. &
                              dtc%vdt2%vdt1%var_f /= 27.0d0 ) error stop 158
   if ( (dtb%vdt2%vdt1%var_g .neqv. .false.) .or. &
                              (dtc%vdt2%vdt1%var_g .neqv. .false.) ) error stop 160
   if ( dtb%vdt2%vdt1%var_h /= 35.0e0 .or. &
                              dtc%vdt2%vdt1%var_h /= 35.0e0 ) error stop 162

!----------------------------------------------------------
!! Test 4

   dta = dt0

   dtp = fun4(dta+dta,dtb)

   call C_F_POINTER(dtp,dtd)

   if ( dtb%var_a /= 5.0e0 .or. dtd%var_a /= 5.0e0 ) error stop 164
   if ( (dtb%var_b .neqv. .false.) .or. (dtd%var_b .neqv. .false.) ) error stop 166
   if ( dtb%var_c /= 13.0q0 .or. dtd%var_c /= 13.0q0 ) error stop 168
   if ( dtb%var_d /= 17.0d0 .or. dtd%var_d /= 17.0d0 ) error stop 170
   if ( dtb%var_e /= 21.0e0 .or. dtd%var_e /= 21.0e0 ) error stop 172
   if ( (dtb%var_f .neqv. .false.) .or. (dtd%var_f .neqv. .false.) ) error stop 174
   if ( dtb%var_g /= 29.0d0 .or. dtd%var_g /= 29.0d0 ) error stop 176
   if ( (dtb%var_h /= 'B') .or. (dtd%var_h /= 'B') ) error stop 178

   if ( dtb%vdt2%var_a /= 6.0d0 .or. dtd%vdt2%var_a /= 6.0d0 ) error stop 180
   if ( (dtb%vdt2%var_b .neqv. .false.) .or. (dtd%vdt2%var_b .neqv. .false.) ) error stop 182
   if ( dtb%vdt2%var_c /= 14.0d0 .or. dtd%vdt2%var_c /= 14.0d0 ) error stop 184
   if ( (dtb%vdt2%var_d /= 'B') .or. (dtd%vdt2%var_d /= 'B') ) error stop 186
   if ( dtb%vdt2%var_e /= 22.0q0 .or. dtd%vdt2%var_e /= 22.0q0 ) error stop 188
   if ( dtb%vdt2%var_f /= 26.0e0 .or. dtd%vdt2%var_f /= 26.0e0 ) error stop 190
   if ( (dtb%vdt2%var_g .neqv. .false.) .or. (dtd%vdt2%var_g .neqv. .false.) ) error stop 192
   if ( dtb%vdt2%var_h /= 34.0e0 .or. dtd%vdt2%var_h /= 34.0e0 ) error stop 194

   if ( (dtb%vdt2%vdt1%var_a .neqv. .false.) .or. &
                              (dtd%vdt2%vdt1%var_a .neqv. .false.) ) error stop 196
   if ( dtb%vdt2%vdt1%var_b /= 11.0d0 .or. &
                              dtd%vdt2%vdt1%var_b /= 11.0d0 ) error stop 198
   if ( (dtb%vdt2%vdt1%var_c /= 'B') .or. &
                              (dtd%vdt2%vdt1%var_c /= 'B') ) error stop 200
   if ( dtb%vdt2%vdt1%var_d /= 19.0e0 .or. &
                              dtd%vdt2%vdt1%var_d /= 19.0e0 ) error stop 202
   if ( dtb%vdt2%vdt1%var_e /= 23.0q0 .or. &
                              dtd%vdt2%vdt1%var_e /= 23.0q0 ) error stop 204
   if ( dtb%vdt2%vdt1%var_f /= 27.0d0 .or. &
                              dtd%vdt2%vdt1%var_f /= 27.0d0 ) error stop 206
   if ( (dtb%vdt2%vdt1%var_g .neqv. .false.) .or. &
                              (dtd%vdt2%vdt1%var_g .neqv. .false.) ) error stop 208
   if ( dtb%vdt2%vdt1%var_h /= 35.0e0 .or. &
                              dtd%vdt2%vdt1%var_h /= 35.0e0 ) error stop 210

!----------------------------------------------------------
end program fxdtylb39

!! Test 1 Fun
function fun1(dtk) bind(c)
   use mxdtylb39
   type(dt3) :: fun1, dtk

   if ( dtk%var_a /= 2.0e0 .or. dtk%vdt2%var_a /= 2.0d0 .or. &
                          (dtk%vdt2%vdt1%var_a .neqv. .true.) ) error stop 212
   if ( (dtk%var_b .neqv. .true.) .or. (dtk%vdt2%var_b .neqv. .true.) .or. &
                          dtk%vdt2%vdt1%var_b /= 4.0d0 ) error stop 214
   if ( dtk%var_c /= 6.0q0 .or. dtk%vdt2%var_c /= 6.0d0 .or. &
                          (dtk%vdt2%vdt1%var_c /= 'A') ) error stop 216
   if ( dtk%var_d /= 8.0d0 .or. (dtk%vdt2%var_d /= 'A') .or. &
                          dtk%vdt2%vdt1%var_d /= 8.0e0 ) error stop 218
   if ( dtk%var_e /= 10.0e0 .or. dtk%vdt2%var_e /= 10.0q0 .or. &
                           dtk%vdt2%vdt1%var_e /= 10.0q0 ) error stop 220
   if ( (dtk%var_f .neqv. .true.) .or. dtk%vdt2%var_f /= 12.0e0 .or. &
                           dtk%vdt2%vdt1%var_f /= 12.0d0 ) error stop 222
   if ( dtk%var_g /= 14.0d0 .or. (dtk%vdt2%var_g .neqv. .true.) .or. &
                           dtk%vdt2%vdt1%var_h /= 16.0e0 ) error stop 224
   if ( (dtk%var_h /= 'A') .or. dtk%vdt2%var_h /= 16.0e0 .or. &
                           dtk%vdt2%vdt1%var_h /= 16.0e0 ) error stop 226

   dtk%var_a = dtk%var_a + 1.0e0
   dtk%var_b = .not. dtk%var_b
   dtk%var_c = dtk%var_c + 1.0q0
   dtk%var_d = dtk%var_d + 1.0d0
   dtk%var_e = dtk%var_e + 1.0e0
   dtk%var_f = .not. dtk%var_f
   dtk%var_g = dtk%var_g + 1.0d0
   dtk%var_h = 'B'

   dtk%vdt2%var_a = dtk%vdt2%var_a + 2.0d0
   dtk%vdt2%var_b = .not. dtk%vdt2%var_b
   dtk%vdt2%var_c = dtk%vdt2%var_c + 2.0d0
   dtk%vdt2%var_d = 'B'
   dtk%vdt2%var_e = dtk%vdt2%var_e + 2.0q0
   dtk%vdt2%var_f = dtk%vdt2%var_f + 2.0e0
   dtk%vdt2%var_g = .not. dtk%vdt2%var_g
   dtk%vdt2%var_h = dtk%vdt2%var_h + 2.0e0

   dtk%vdt2%vdt1%var_a = .not. dtk%vdt2%vdt1%var_a
   dtk%vdt2%vdt1%var_b = dtk%vdt2%vdt1%var_b + 3.0d0
   dtk%vdt2%vdt1%var_c = 'B'
   dtk%vdt2%vdt1%var_d = dtk%vdt2%vdt1%var_d + 3.0e0
   dtk%vdt2%vdt1%var_e = dtk%vdt2%vdt1%var_e + 3.0q0
   dtk%vdt2%vdt1%var_f = dtk%vdt2%vdt1%var_f + 3.0d0
   dtk%vdt2%vdt1%var_g = .not. dtk%vdt2%vdt1%var_g
   dtk%vdt2%vdt1%var_h = dtk%vdt2%vdt1%var_h + 3.0e0

   fun1 = dtk

end function fun1
!----------------------------------------------------------

!! Test 2 Fun
function fun2(dtk) bind(c)
   use mxdtylb39
   type(dt3), value :: dtk
   type(dt3) :: fun2

   if ( dtk%var_a /= 2.0e0 .or. dtk%vdt2%var_a /= 2.0d0 .or. &
                          (dtk%vdt2%vdt1%var_a .neqv. .true.) ) error stop 228
   if ( (dtk%var_b .neqv. .true.) .or. (dtk%vdt2%var_b .neqv. .true.) .or. &
                          dtk%vdt2%vdt1%var_b /= 4.0d0 ) error stop 230
   if ( dtk%var_c /= 6.0q0 .or. dtk%vdt2%var_c /= 6.0d0 .or. &
                          (dtk%vdt2%vdt1%var_c /= 'A') ) error stop 232
   if ( dtk%var_d /= 8.0d0 .or. (dtk%vdt2%var_d /= 'A') .or. &
                          dtk%vdt2%vdt1%var_d /= 8.0e0 ) error stop 234
   if ( dtk%var_e /= 10.0e0 .or. dtk%vdt2%var_e /= 10.0q0 .or. &
                           dtk%vdt2%vdt1%var_e /= 10.0q0 ) error stop 236
   if ( (dtk%var_f .neqv. .true.) .or. dtk%vdt2%var_f /= 12.0e0 .or. &
                           dtk%vdt2%vdt1%var_f /= 12.0d0 ) error stop 238
   if ( dtk%var_g /= 14.0d0 .or. (dtk%vdt2%var_g .neqv. .true.) .or. &
                           dtk%vdt2%vdt1%var_h /= 16.0e0 ) error stop 240
   if ( (dtk%var_h /= 'A') .or. dtk%vdt2%var_h /= 16.0e0 .or. &
                           dtk%vdt2%vdt1%var_h /= 16.0e0 ) error stop 242

   dtk%var_a = dtk%var_a + 1.0e0
   dtk%var_b = .not. dtk%var_b
   dtk%var_c = dtk%var_c + 1.0q0
   dtk%var_d = dtk%var_d + 1.0d0
   dtk%var_e = dtk%var_e + 1.0e0
   dtk%var_f = .not. dtk%var_f
   dtk%var_g = dtk%var_g + 1.0d0
   dtk%var_h = 'B'

   dtk%vdt2%var_a = dtk%vdt2%var_a + 2.0d0
   dtk%vdt2%var_b = .not. dtk%vdt2%var_b
   dtk%vdt2%var_c = dtk%vdt2%var_c + 2.0d0
   dtk%vdt2%var_d = 'B'
   dtk%vdt2%var_e = dtk%vdt2%var_e + 2.0q0
   dtk%vdt2%var_f = dtk%vdt2%var_f + 2.0e0
   dtk%vdt2%var_g = .not. dtk%vdt2%var_g
   dtk%vdt2%var_h = dtk%vdt2%var_h + 2.0e0

   dtk%vdt2%vdt1%var_a = .not. dtk%vdt2%vdt1%var_a
   dtk%vdt2%vdt1%var_b = dtk%vdt2%vdt1%var_b + 3.0d0
   dtk%vdt2%vdt1%var_c = 'B'
   dtk%vdt2%vdt1%var_d = dtk%vdt2%vdt1%var_d + 3.0e0
   dtk%vdt2%vdt1%var_e = dtk%vdt2%vdt1%var_e + 3.0q0
   dtk%vdt2%vdt1%var_f = dtk%vdt2%vdt1%var_f + 3.0d0
   dtk%vdt2%vdt1%var_g = .not. dtk%vdt2%vdt1%var_g
   dtk%vdt2%vdt1%var_h = dtk%vdt2%vdt1%var_h + 3.0e0

   fun2 = dtk

end function fun2
!----------------------------------------------------------

!! Test 3 Fun
function fun3(dtu,dty) bind(c)
   use mxdtylb39
   type(dt3), intent(in) :: dtu
   type(dt3), intent(out) :: dty
   type(dt3) :: fun3

   if ( dtu%var_a /= 4.0e0 .or. dtu%vdt2%var_a /= 4.0d0 .or. &
                            (dtu%vdt2%vdt1%var_a .neqv. .true.) ) error stop 244
   if ( (dtu%var_b .neqv. .true.) .or. (dtu%vdt2%var_b .neqv. .true.) .or. &
                            dtu%vdt2%vdt1%var_b /= 8.0d0 ) error stop 246
   if ( dtu%var_c /= 12.0q0 .or. dtu%vdt2%var_c /= 12.0d0 .or. &
                             (dtu%vdt2%vdt1%var_c /= 'A') ) error stop 248
   if ( dtu%var_d /= 16.0d0 .or. (dtu%vdt2%var_d /= 'A') .or. &
                             dtu%vdt2%vdt1%var_d /= 16.0e0 ) error stop 250
   if ( dtu%var_e /= 20.0e0 .or. dtu%vdt2%var_e /= 20.0q0 .or. &
                             dtu%vdt2%vdt1%var_e /= 20.0q0 ) error stop 252
   if ( (dtu%var_f .neqv. .true.) .or. dtu%vdt2%var_f /= 24.0e0 .or. &
                             dtu%vdt2%vdt1%var_f /= 24.0d0 ) error stop 254
   if ( dtu%var_g /= 28.0d0 .or. (dtu%vdt2%var_g .neqv. .true.) .or. &
                             (dtu%vdt2%vdt1%var_g .neqv. .true.) ) error stop 256
   if ( (dtu%var_h /= 'A') .or. dtu%vdt2%var_h /= 32.0e0 .or. &
                             dtu%vdt2%vdt1%var_h /= 32.0e0 ) error stop 258

   dty%var_a = dtu%var_a + 1.0e0
   dty%var_b = .not. dtu%var_b
   dty%var_c = dtu%var_c + 1.0q0
   dty%var_d = dtu%var_d + 1.0d0
   dty%var_e = dtu%var_e + 1.0e0
   dty%var_f = .not. dtu%var_f
   dty%var_g = dtu%var_g + 1.0d0
   dty%var_h = 'B'

   dty%vdt2%var_a = dtu%vdt2%var_a + 2.0d0
   dty%vdt2%var_b = .not. dtu%vdt2%var_b
   dty%vdt2%var_c = dtu%vdt2%var_c + 2.0d0
   dty%vdt2%var_d = 'B'
   dty%vdt2%var_e = dtu%vdt2%var_e + 2.0q0
   dty%vdt2%var_f = dtu%vdt2%var_f + 2.0e0
   dty%vdt2%var_g = .not. dtu%vdt2%var_g
   dty%vdt2%var_h = dtu%vdt2%var_h + 2.0e0

   dty%vdt2%vdt1%var_a = .not. dtu%vdt2%vdt1%var_a
   dty%vdt2%vdt1%var_b = dtu%vdt2%vdt1%var_b + 3.0d0
   dty%vdt2%vdt1%var_c = 'B'
   dty%vdt2%vdt1%var_d = dtu%vdt2%vdt1%var_d + 3.0e0
   dty%vdt2%vdt1%var_e = dtu%vdt2%vdt1%var_e + 3.0q0
   dty%vdt2%vdt1%var_f = dtu%vdt2%vdt1%var_f + 3.0d0
   dty%vdt2%vdt1%var_g = .not. dtu%vdt2%vdt1%var_g
   dty%vdt2%vdt1%var_h = dtu%vdt2%vdt1%var_h + 3.0e0

   fun3 = dty

end function fun3
!----------------------------------------------------------

!! Test 4 Fun
function fun4(dtu,dty) bind(c)
   use mxdtylb39
   type(dt3), intent(in) :: dtu
   type(dt3), intent(out) :: dty
   type(dt3), target, save :: dtz
   type(C_PTR) :: fun4

   if ( dtu%var_a /= 4.0e0 .or. dtu%vdt2%var_a /= 4.0d0 .or. &
                            (dtu%vdt2%vdt1%var_a .neqv. .true.) ) error stop 260
   if ( (dtu%var_b .neqv. .true.) .or. (dtu%vdt2%var_b .neqv. .true.) .or. &
                            dtu%vdt2%vdt1%var_b /= 8.0d0 ) error stop 262
   if ( dtu%var_c /= 12.0q0 .or. dtu%vdt2%var_c /= 12.0d0 .or. &
                             (dtu%vdt2%vdt1%var_c /= 'A') ) error stop 264
   if ( dtu%var_d /= 16.0d0 .or. (dtu%vdt2%var_d /= 'A') .or. &
                             dtu%vdt2%vdt1%var_d /= 16.0e0 ) error stop 266
   if ( dtu%var_e /= 20.0e0 .or. dtu%vdt2%var_e /= 20.0q0 .or. &
                             dtu%vdt2%vdt1%var_e /= 20.0q0 ) error stop 268
   if ( (dtu%var_f .neqv. .true.) .or. dtu%vdt2%var_f /= 24.0e0 .or. &
                             dtu%vdt2%vdt1%var_f /= 24.0d0 ) error stop 270
   if ( dtu%var_g /= 28.0d0 .or. (dtu%vdt2%var_g .neqv. .true.) .or. &
                             (dtu%vdt2%vdt1%var_g .neqv. .true.) ) error stop 272
   if ( (dtu%var_h /= 'A') .or. dtu%vdt2%var_h /= 32.0e0 .or. &
                             dtu%vdt2%vdt1%var_h /= 32.0e0 ) error stop 274

   dtz%var_a = dtu%var_a + 1.0e0
   dtz%var_b = .not. dtu%var_b
   dtz%var_c = dtu%var_c + 1.0q0
   dtz%var_d = dtu%var_d + 1.0d0
   dtz%var_e = dtu%var_e + 1.0e0
   dtz%var_f = .not. dtu%var_f
   dtz%var_g = dtu%var_g + 1.0d0
   dtz%var_h = 'B'

   dtz%vdt2%var_a = dtu%vdt2%var_a + 2.0d0
   dtz%vdt2%var_b = .not. dtu%vdt2%var_b
   dtz%vdt2%var_c = dtu%vdt2%var_c + 2.0d0
   dtz%vdt2%var_d = 'B'
   dtz%vdt2%var_e = dtu%vdt2%var_e + 2.0q0
   dtz%vdt2%var_f = dtu%vdt2%var_f + 2.0e0
   dtz%vdt2%var_g = .not. dtu%vdt2%var_g
   dtz%vdt2%var_h = dtu%vdt2%var_h + 2.0e0

   dtz%vdt2%vdt1%var_a = .not. dtu%vdt2%vdt1%var_a
   dtz%vdt2%vdt1%var_b = dtu%vdt2%vdt1%var_b + 3.0d0
   dtz%vdt2%vdt1%var_c = 'B'
   dtz%vdt2%vdt1%var_d = dtu%vdt2%vdt1%var_d + 3.0e0
   dtz%vdt2%vdt1%var_e = dtu%vdt2%vdt1%var_e + 3.0q0
   dtz%vdt2%vdt1%var_f = dtu%vdt2%vdt1%var_f + 3.0d0
   dtz%vdt2%vdt1%var_g = .not. dtu%vdt2%vdt1%var_g
   dtz%vdt2%vdt1%var_h = dtu%vdt2%vdt1%var_h + 3.0e0

   dty = dtz

   fun4 = C_LOC(dtz)

end function fun4
!----------------------------------------------------------
