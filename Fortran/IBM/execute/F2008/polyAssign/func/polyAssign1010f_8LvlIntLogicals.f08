! *********************************************************************
!* ===================================================================
!*
!* FEATURE                      : F2008: LHS of intrinsic assignment is allowed to be polymorphic (96086)
!*                                https://compjazz.torolab.ibm.com:9443/jazz/resource/itemName/com.ibm.team.workitem.WorkItem/96086
!* DATE                         : 07 August 2015
!*
!* PRIMARY FUNCTIONS TESTED     : F2008: LHS of intrinsic assignment is allowed to be polymorphic
!*
!* REQUIRED COMPILER OPTIONS    :
!*
!* DESCRIPTION                  : Test whether the variable of an intrinsic assignment is polymorphic.
!*                              : Variables should be allocated and assigned with the corresponding dynamic type.
!*                              : We test polymorphic assignment to eitht levels of extensible derived types described as bellow.
!*                              : Use integer*1, integer*2, integer*4, integer*8 to the deriveed type to test polymorphic assignment with different integers.
!*                              : Use logical*1, logical*2, logical*4, logical*8  to the derived type to test polymorphic assignment with different kinds of logical types.
!*---------------------------------------------------------------------
!* t is the base derived type, and t2 and t3 are extensibe derived
!* t2 and t3 are extensibe derived type of t
!* t4 and t5 are extensibe derived type of t3
!* t6 and t7 are extensibe derived type of t5
!* t8 and t9 are extensibe derived type of t7
!* t10 and t11 are extensibe derived type of t9
!* t12 and t13 are extensibe derived type of t11
!* t14 and t15 are extensibe derived type of t13
!*
!*                           t
!*                          / \
!*                         /   \
!*                        /     \
!*                       t2     t3
!*                              /\
!*                             /  \
!*                            /    \
!*                           t4    t5
!*                                 /\
!*                                /  \
!*                               /    \
!*                              t6    t7
!*                                    /\
!*                                   /  \
!*                                  /    \
!*                                 t8    t9
!*                                       /\
!*                                      /  \
!*                                     /    \
!*                                   t10    t11
!*                                          /\
!*                                         /  \
!*                                        /    \
!*                                      t12    t13
!*                                             /\
!*                                            /  \
!*                                           /    \
!*                                         t14    t15
!---------------------------------------------------------------------
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  08/12/15    AL     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890

Program polyAssign1010f
    Type t
      integer :: i = 10
    End Type
    Type, Extends(t) :: t2
    End Type
    Type, Extends(t) :: t3
       integer :: j3 = 30
    End Type
    Type, Extends(t3) :: t4
       integer :: j4 = 40
    End Type
    Type, Extends(t3) :: t5
       integer :: j5 = 50
    End Type
    Type, Extends(t5) :: t6
       integer :: j6 = 66
    End Type
    Type, Extends(t5) :: t7
       integer :: j7 = 77
    End Type
    Type, Extends(t7) :: t8
       integer :: j8 = 88
    End Type
    Type, Extends(t7) :: t9
       integer :: j9 = 99
    End Type
    Type, Extends(t9) :: t10
       integer :: j10 = 1010
    End Type
    Type, Extends(t9) :: t11
       integer :: j11 = 1111
    End Type
    Type, Extends(t11) :: t12
       integer :: j12 = 1212
    End Type
    Type, Extends(t11) :: t13
       integer :: j13 = 1313
    End Type
    Type, Extends(t13) :: t14
       integer :: j14 = 1414
       integer*1 :: m1 = 11
       integer*2 :: m2 = 22
       integer*4 :: m3 = 44
       integer*8 :: m4 = 88
       logical*1 :: lm1 = .true.
       logical*2 :: lm2 = .false.
       logical*4 :: lm3 = .true.
       logical*8 :: lm4 = .false.
    End Type
    Type, Extends(t13) :: t15
       integer :: j15 = 1515
       integer*1 :: k1 = 11
       integer*2 :: k2 = 22
       integer*4 :: k3 = 44
       integer*8 :: k4 = 88
       logical*1 :: lk1 = .true.
       logical*2 :: lk2 = .false.
       logical*4 :: lk3 = .true.
       logical*8 :: lk4 = .false.
    End Type
    Class(t), Allocatable :: x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15
    x1 = t()
    if (x1%i .ne. 10) error stop 1
    x2 = t2()
    if (x2%i .ne. 10) error stop 2
    x2 = t2(11)
    if (x2%i .ne. 11) error stop 3
    x3 = t3()
    if (x3%i .ne. 10) error stop 4
    x4 = t4()
    if (x4%i .ne. 10) error stop 5
    x5 = t5()
    if (x5%i .ne. 10) error stop 6
    x6 = t6()
    if (x6%i .ne. 10) error stop 7
    x7 = t7()
    if (x7%i .ne. 10) error stop 8
    x8 = t8()
    if (x8%i .ne. 10) error stop 9
    x9 = t9()
    if (x9%i .ne. 10) error stop 10
    x10 = t10()
    if (x10%i .ne. 10) error stop 11
    x11 = t11()
    if (x11%i .ne. 10) error stop 12
    x12 = t12()
    if (x12%i .ne. 10) error stop 13
    x13 = t13()
    if (x13%i .ne. 10) error stop 14
    x14 = t14()
    if (x14%i .ne. 10) error stop 15
    x15 = t15()
    if (x15%i .ne. 10) error stop 16
    select type (x4)
      type is (t)
        error stop 17
      type is (t3)
        error stop 18
      type is (t4)
        if (x4%i .ne. 10) error stop 19
        if (x4%j3 .ne. 30) error stop 20
        if (x4%j4 .ne. 40) error stop 21
      class default
        error stop  22
    end select
    x5 = t5()
    if (x5%i .ne. 10) error stop 23
    select type (x5)
      type is (t)
        error stop 24
      type is (t3)
        error stop 25
      type is (t5)
        if (x5%i .ne. 10) error stop 26
        if (x5%j3 .ne. 30) error stop 27
        if (x5%j5 .ne. 50) error stop 28
      class default
        error stop 29
    end select
    x5 = t4()
    if (x5%i .ne. 10) error stop 30
    select type (x5)
      type is (t)
        error stop 31
      type is (t3)
        error stop 32
      type is (t4)
        if (x5%i .ne. 10) error stop 33
        if (x5%j3 .ne. 30) error stop 34
        if (x5%j4 .ne. 40) error stop 35
      type is (t5)
        error stop 36
      class default
        error stop 37
    end select
    x7 = t7()
    select type (x7)
      type is (t)
        error stop 38
      type is (t3)
        error stop 39
      type is (t5)
        error stop 40
      type is (t7)
        if (x7%i .ne. 10) error stop 41
        if (x7%j3 .ne. 30) error stop 42
        if (x7%j5 .ne. 50) error stop 43
        if (x7%j7 .ne. 77) error stop 44
      class default
        error stop 45
    end select
    x7 = t6()
    select type (x7)
      type is (t)
        error stop 46
      type is (t3)
        error stop 47
      type is (t4)
        error stop 48
      type is (t5)
        error stop 49
      type is (t6)
        if (x7%i .ne. 10) error stop 50
        if (x7%j3 .ne. 30) error stop 51
        if (x7%j5 .ne. 50) error stop 52
        if (x7%j6 .ne. 66) error stop 53
      type is (t7)
        error stop 54
      class default
        error stop 55
    end select
    x9 = t9()
    select type (x9)
      type is (t)
        error stop 56
      type is (t3)
        error stop 57
      type is (t5)
        error stop 58
      type is (t7)
        error stop 59
      type is (t9)
        if (x9%i .ne. 10) error stop 60
        if (x9%j3 .ne. 30) error stop 61
        if (x9%j5 .ne. 50) error stop 62
        if (x9%j7 .ne. 77) error stop 63
        if (x9%j9 .ne. 99) error stop 64
      class default
        error stop 65
    end select
    x9 = t8()
    select type (x9)
      type is (t)
        error stop 66
      type is (t3)
        error stop 67
      type is (t4)
        error stop 68
      type is (t5)
        error stop 69
      type is (t6)
        error stop 70
      type is (t7)
        error stop 71
      type is (t8)
        if (x9%i .ne. 10) error stop 72
        if (x9%j3 .ne. 30) error stop 73
        if (x9%j5 .ne. 50) error stop 74
        if (x9%j7 .ne. 77) error stop 75
        if (x9%j8 .ne. 88) error stop 76
      class default
        error stop 77
    end select
    x10 = t10()
    select type (x10)
      type is (t)
        error stop 78
      type is (t3)
        error stop 79
      type is (t5)
        error stop 80
      type is (t7)
        error stop 81
      type is (t9)
        error stop 82
      type is (t10)
        if (x10%i .ne. 10) error stop 83
        if (x10%j3 .ne. 30) error stop 84
        if (x10%j5 .ne. 50) error stop 85
        if (x10%j7 .ne. 77) error stop 86
        if (x10%j9 .ne. 99) error stop 87
        if (x10%j10 .ne. 1010) error stop 88
      class default
        error stop 89
    end select
    x11 = t11()
    select type (x11)
      type is (t)
        error stop 90
      type is (t3)
        error stop 91
      type is (t5)
        error stop 92
      type is (t7)
        error stop 93
      type is (t9)
        error stop 94
      type is (t10)
        error stop 95
      type is (t11)
        if (x11%i .ne. 10) error stop 96
        if (x11%j3 .ne. 30) error stop 97
        if (x11%j5 .ne. 50) error stop 98
        if (x11%j7 .ne. 77) error stop 99
        if (x11%j9 .ne. 99) error stop 100
        if (x11%j11 .ne. 1111) error stop 101
      class default
        error stop 102
    end select
    x11 = t10()
    if (x11%i .ne. 10) error stop 103
    select type (x11)
      type is (t)
        error stop 104
      type is (t3)
        error stop 105
      type is (t5)
        error stop 106
      type is (t7)
        error stop 107
      type is (t9)
        error stop 108
      type is (t10)
        if (x11%i .ne. 10) error stop 109
        if (x11%j3 .ne. 30) error stop 110
        if (x11%j5 .ne. 50) error stop 111
        if (x11%j7 .ne. 77) error stop 112
        if (x11%j9 .ne. 99) error stop 113
        if (x11%j10 .ne. 1010) error stop 114
      type is (t11)
        error stop 115
      class default
        error stop 116
    end select
    x13 = t13()
    if (x13%i .ne. 10) error stop 117
    select type (x13)
      type is (t)
        error stop 118
      type is (t3)
        error stop 119
      type is (t5)
        error stop 120
      type is (t7)
        error stop 121
      type is (t9)
        error stop 122
      type is (t10)
        error stop 123
      type is (t11)
        error stop 124
      type is (t13)
        if (x13%i .ne. 10) error stop 125
        if (x13%j3 .ne. 30) error stop 126
        if (x13%j5 .ne. 50) error stop 127
        if (x13%j7 .ne. 77) error stop 128
        if (x13%j9 .ne. 99) error stop 129
        if (x13%j11 .ne. 1111) error stop 131
        if (x13%j13 .ne. 1313) error stop 132
      class default
        error stop 133
    end select
    x13 = t12()
    if (x13%i .ne. 10) error stop 134
    select type (x13)
      type is (t)
        error stop 135
      type is (t3)
        error stop 136
      type is (t5)
        error stop 137
      type is (t7)
        error stop 138
      type is (t9)
        error stop 139
      type is (t10)
        error stop 140
      type is (t11)
        error stop 141
      type is (t12)
        if (x13%i .ne. 10) error stop 142
        if (x13%j3 .ne. 30) error stop 143
        if (x13%j5 .ne. 50) error stop 145
        if (x13%j7 .ne. 77) error stop 146
        if (x13%j9 .ne. 99) error stop 147
        if (x13%j11 .ne. 1111) error stop 148
        if (x13%j12 .ne. 1212) error stop 149
      type is (t13)
        error stop 150
      class default
        error stop 151
    end select
    x14 = t15()
    if (x14%i .ne. 10) error stop 152
    select type (x14)
      type is (t)
        error stop 153
      type is (t3)
        error stop 154
      type is (t5)
        error stop 155
      type is (t7)
        error stop 156
      type is (t9)
        error stop 157
      type is (t10)
        error stop 158
      type is (t11)
        error stop 159
      type is (t12)
        error stop 160
      type is (t13)
        error stop 161
      type is (t14)
        error stop 162
      type is (t15)
        if (x14%i .ne. 10) error stop 163
        if (x14%j3 .ne. 30) error stop 164
        if (x14%j5 .ne. 50) error stop 165
        if (x14%j7 .ne. 77) error stop 166
        if (x14%j9 .ne. 99) error stop 167
        if (x14%j11 .ne. 1111) error stop 168
        if (x14%j13 .ne. 1313) error stop 169
        if (x14%j15 .ne. 1515) error stop 170
        if (x14%k1 .ne. 11) error stop 171
        if (x14%k2 .ne. 22) error stop 172
        if (x14%k3 .ne. 44) error stop 173
        if (x14%k4 .ne. 88) error stop 174
        if (x14%lk1 .neqv. .true.) error stop 175
        if (x14%lk2 .neqv. .false.) error stop 176
        if (x14%lk3 .neqv. .true.) error stop 177
        if (x14%lk4 .neqv. .false.) error stop 178
      class default
        error stop 181
    end select
    x14 = t14()
    if (x14%i .ne. 10) error stop 182
    select type (x14)
      type is (t)
        error stop 183
      type is (t3)
        error stop 184
      type is (t5)
        error stop 185
      type is (t7)
        error stop 186
      type is (t9)
        error stop 187
      type is (t10)
        error stop 188
      type is (t11)
        error stop 189
      type is (t12)
        error stop 190
      type is (t13)
        error stop 191
      type is (t14)
        if (x14%i .ne. 10) error stop 192
        if (x14%j3 .ne. 30) error stop 193
        if (x14%j5 .ne. 50) error stop 194
        if (x14%j7 .ne. 77) error stop 196
        if (x14%j9 .ne. 99) error stop 197
        if (x14%j11 .ne. 1111) error stop 198
        if (x14%j13 .ne. 1313) error stop 199
        if (x14%j14 .ne. 1414) error stop 200
        if (x14%m1 .ne. 11) error stop 201
        if (x14%m2 .ne. 22) error stop 202
        if (x14%m3 .ne. 44) error stop 203
        if (x14%m4 .ne. 88) error stop 204
        if (x14%lm1 .neqv. .true.) error stop 205
        if (x14%lm2 .neqv. .false.) error stop 206
        if (x14%lm3 .neqv. .true.) error stop 207
        if (x14%lm4 .neqv. .false.) error stop 208
      type is (t15)
        error stop 211
      class default
        error stop 212
    end select
    x15 = t14()
    if (x15%i .ne. 10) error stop 213
    select type (x15)
      type is (t)
        if (x15%i .ne. 10) error stop 214
      type is (t3)
        error stop 215
      type is (t5)
        error stop 216
      type is (t7)
        error stop 217
      type is (t9)
        error stop 218
      type is (t10)
        error stop 219
      type is (t11)
        error stop 220
      type is (t12)
        error stop 221
      type is (t13)
        error stop 222
      type is (t14)
        if (x15%i .ne. 10) error stop 223
        if (x15%j3 .ne. 30) error stop 224
        if (x15%j5 .ne. 50) error stop 225
        if (x15%j7 .ne. 77) error stop 226
        if (x15%j9 .ne. 99) error stop 227
        if (x15%j11 .ne. 1111) error stop 228
        if (x15%j13 .ne. 1313) error stop 229
        if (x15%j14 .ne. 1414) error stop 230
        if (x15%m1 .ne. 11) error stop 231
        if (x15%m2 .ne. 22) error stop 232
        if (x15%m3 .ne. 44) error stop 233
        if (x15%m4 .ne. 88) error stop 234
        if (x15%lm1 .neqv. .true.) error stop 235
        if (x15%lm2 .neqv. .false.) error stop 236
        if (x15%lm3 .neqv. .true.) error stop 237
        if (x15%lm4 .neqv. .false.) error stop 238
      type is (t15)
        error stop 241
      class default
        error stop 242
    end select
    x15 = t15()
    if (x15%i .ne. 10) error stop 243
    select type (x15)
      type is (t)
        error stop 244
      type is (t3)
        error stop 245
      type is (t5)
        error stop 246
      type is (t7)
        error stop 247
      type is (t9)
        error stop 248
      type is (t10)
        error stop 249
      type is (t11)
        error stop 250
      type is (t12)
        error stop 251
      type is (t13)
        error stop 252
      type is (t14)
        error stop 253
      type is (t15)
        if (x15%i .ne. 10) error stop 254
        if (x15%j3 .ne. 30) error stop 255
        if (x15%j5 .ne. 50) error stop 256
        if (x15%j7 .ne. 77) error stop 257
        if (x15%j9 .ne. 99) error stop 258
        if (x15%j11 .ne. 1111) error stop 259
        if (x15%j13 .ne. 1313) error stop 260
        if (x15%j15 .ne. 1515) error stop 261
        if (x15%k1 .ne. 11) error stop 262
        if (x15%k2 .ne. 22) error stop 263
        if (x15%k3 .ne. 44) error stop 264
        if (x15%k4 .ne. 88) error stop 265
        if (x15%lk1 .neqv. .true.) error stop 266
        if (x15%lk2 .neqv. .false.) error stop 267
        if (x15%lk3 .neqv. .true.) error stop 268
        if (x15%lk4 .neqv. .false.) error stop 269
      class default
        error stop 272
    end select
End Program