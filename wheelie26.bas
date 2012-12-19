' ------------------------------------------------------------------------------
  _____ _     _   _              _ _ _ _           _ _     
' |   __| |___| |_| |_ ___ ___   | | | | |_ ___ ___| |_|___ 
' |   __| | -_| '_|  _| . |  _|  | | | |   | -_| -_| | | -_|
' |_____|_|___|_,_|_| |___|_|    |_____|_|_|___|___|_|_|___|
'
'  080718-1 (c)2009 Elektor International Media B.V. / Chris Krohne
' ------------------------------------------------------------------------------
' ------------------------------------------------------------------------------
' Purpose
'   Elektor-Wheelie motor control board source code
'
' Changes Log
'  - 2.6 : final beta source code for publication
'  - 2.4b : initial beta source code for publication
'
' Notes
'  Nothing special, may come later...
' ------------------------------------------------------------------------------
'modified on 2011-04-18 for the change of ADXL330(Sensitivity: 300 mV/G) instead of ADXL320(Sensitivity: 174 mV/G)
' ------------------------------------------------------------------------------



$crystal = 16000000
$baud = 19200







Pwm_al Alias Ocr1al
Pwm_bl Alias Ocr1bl

Led3 Alias Portb.4
Led2 Alias Portb.3
Led1 Alias Portb.2
Cw_ccw_a Alias Portd.6
Cw_ccw_b Alias Portd.7



Config Porta = Input
Config Portb = Input
Config Pinb.4 = Output
Config Pinb.3 = Output
Config Pinb.2 = Output


Config Portc = Output
Config Portd = Output
Config Pind.2 = Input

Config Adc = Single , Prescaler = 128 , Reference = Off
Start Adc
Config Timer0 = Timer , Prescale = 1024
On Timer0 Tinter




Dim Buzzer As Byte
Dim Ad_adxl As Long
Dim Ad_gyro As Long
Dim Ad_batt As Integer
Dim Ad_swi As Integer
Dim Total_adxl_gyro As Long
Dim Average_gyro As Long
Dim Average_batt As Long
Dim Drivea As Integer
Dim Driveb As Integer
Dim Buf1 As Long
Dim Buf As Long
Dim Buf2 As Long
Dim Buf5 As Integer
Dim Tilt_angle As Long
Dim Drive_a As Integer
Dim Drive_b As Integer
Dim Tcount As Integer
Dim Drivespeed As Long

Dim Steeringsignal As Long
Dim Anglecorrection As Long
Dim Angle_rate As Long

Dim Balance_moment As Long


Dim Mmode As Byte

Dim Drive_sum As Long
Dim Ad_rocker As Integer

Dim Blinkcount As Byte
Dim Timeout As Long
Dim Delta As Byte
Dim Buf3 As Long
Dim Rockersq As Long

Dim Rocker_zero As Integer
Dim Adxl_zero As Word
Dim Gyro_zero As Word
Dim Loopct As Integer
Dim Limitcurr As Integer
Dim Errno As Byte
Dim Adxl_offset As Integer



Const First_adc_input = 0
Const Last_adc_input = 6
Const Adc_vref_type = $0x40


Const Mdrivesumlimit = 20000

Const Total_looptime = 500                                  ' / / Looptime For Filters
Const Total_looptime10 = 50                                 ' / / Looptime For Filters

Const _run = 1
Const _standby = 0
Const _warmup = 2
Const _down = 3

Const Safespeed = 5000
Const Msafespeed = -5000
Const Sw_down = 50
Const Critical = 80



Const Max_pwm = 180
Const Mmax_pwm = -180



Const Battdead = 487000
Const Battok = 505000










Declare Sub Get_tilt_angle
Declare Sub Set_pwm
Declare Sub Algo
Declare Sub Process
Declare Sub Ini
Declare Sub Getspeedlimit
Declare Sub Err_value



Pwm_al = 255
Pwm_bl = 0


'Init Variables'
Gosub Ini


' Give some blinks
Set Led1
Waitms 150
Set Led2
Waitms 150
Set Led3
Waitms 250
Reset Led1
Reset Led2
Reset Led3



'---------------------------------------------------------'
'check pushbutton'
Ad_swi = 0
For Buf = 1 To 10
  Ad_swi = Ad_swi + Getadc(7)
  Waitms 1
Next Buf
'calculate middel'
Ad_swi = Ad_swi / 10
Waitms 100

'must be closed
If Ad_swi < 100 Then
  Errno = 4
  Goto Err_value
End If


'---------------------------------------------------------'
'Get Steering_zero Position'
Ad_rocker = 0
For Buf = 1 To 10
  Ad_rocker = Ad_rocker + Getadc(6)
  Waitms 1
Next Buf
'calculate middel'
Rocker_zero = Ad_rocker / 10


Waitms 100
'---------------------------------------------------------'
'Get gyro_zero
Ad_gyro = 0
For Buf = 1 To 10
  Buf1 = Getadc(5)
  Buf1 = 1024 - Buf1
  Ad_gyro = Ad_gyro + Buf1
  Waitms 5
Next Buf
'calculate middel'
Gyro_zero = Ad_gyro / 10
'set average_gyro
Average_gyro = Total_looptime * Gyro_zero


'---------------------------------------------------------'
'Get adxl_zero
Ad_adxl = 0
For Buf = 1 To 10
  Buf1 = Getadc(3)
  Ad_adxl = Ad_adxl + Buf1
  Waitms 5
Next Buf
'calculate middel'
Adxl_zero = Ad_adxl / 10




' check if in rage
If Adxl_zero < 310 Then   'modified from 400
  Errno = 3
  Goto Err_value
End If

If Adxl_zero > 710 Then   'modified from 600
  Errno = 3
  Goto Err_value
End If

If Gyro_zero < 350 Then
  Errno = 2
  Goto Err_value
End If
If Gyro_zero > 750 Then
  Errno = 2
  Goto Err_value
End If





Enable Interrupts
Enable Timer0


'main loop
S1:

' nothing to do, all work is done in the interrupt ;)





Goto S1
'----------------------------------------------------------'









'----------------------------------------------------------'

Sub Set_pwm
    'Limiting PWM'
    If Drive_a > Max_pwm Then
      Drive_a = Max_pwm
    End If
    If Drive_a < Mmax_pwm Then
       Drive_a = Mmax_pwm
    End If





    'set direction bit'
    If Drive_a < 0 Then
       Drivea = Drive_a * -1
       Cw_ccw_a = 1
    End If
    If Drive_a >= 0 Then
        Drivea = Drive_a
        Cw_ccw_a = 0
    End If
    'Inverse signal to have 180? phaseshift to PWMB

    Pwm_al = 255 - Drivea


    'Limiting PWM'
    If Drive_b > Max_pwm Then
       Drive_b = Max_pwm
    End If
    If Drive_b < Mmax_pwm Then
       Drive_b = Mmax_pwm
    End If


    'set direction bit'
    If Drive_b < 0 Then
        Driveb = Drive_b * -1
        Cw_ccw_b = 1
    End If
    If Drive_b >= 0 Then
        Driveb = Drive_b
        Cw_ccw_b = 0
    End If

    Pwm_bl = Driveb
    Return
End Sub

'----------------------------------------------------------'



'----------------------------------------------------------'

Sub Algo
    'Buf = Tilt_angle + Anglecorrection
    Buf = Tilt_angle
    Buf = Buf * 17                                          '19
    Buf1 = Angle_rate * 17                                  ' 19

    'calculate balance moment
    Balance_moment = Buf1 + Buf

    'calculate drive_sum
    Drive_sum = Drive_sum + Balance_moment

    ' limitting
    If Drive_sum > 55000 Then
      Drive_sum = 55000
    End If
    If Drive_sum < -55000 Then
      Drive_sum = -55000
    End If


    'calculate drive speed'
    Buf = Drive_sum / 155
    Buf1 = Balance_moment / 165
    Drivespeed = Buf + Buf1

    Return

End Sub

'----------------------------------------------------------'


'----------------------------------------------------------'

Sub Getspeedlimit

 Buf1 = 0
 If Drive_sum > 0 Then
   Buf1 = Drive_sum - Mdrivesumlimit
 End If

 Buf1 = Buf1 / 70
 Adxl_offset = 0

 If Buf1 > 13 Then Buf1 = 13
 If Buf1 < 0 Then Buf1 = 0

 If Buf1 > 0 Then
  Adxl_offset = Buf1
  Buzzer = 2
 End If





 If Buzzer = 0 Then
   'check batt voltage
   If Average_batt > Battok Then
     Set Led1
     Reset Led2
     Reset Led3
   End If
   If Average_batt < Battok Then
     Set Led2
     Reset Led3
     Reset Led1
   End If

   If Average_batt < Battdead Then
     Reset Led2
     Reset Led1
     Set Led3
   End If


 End If


Return



End Sub
'----------------------------------------------------------'



'----------------------------------------------------------'

Sub Process

 Tcount = Tcount + 1
 'do some logs or something else
 If Tcount > 10 Then
   If Buzzer = 1 Then
     Set Led1
     Set Led2
     Set Led3
   End If

   If Buzzer = 2 Then
     Set Led3
   End If

 End If
 If Tcount > 16 Then
   If Buzzer > 0 Then
     Reset Led1
     Reset Led2
     Reset Led3
     Blinkcount = Blinkcount + 1
   End If

   Tcount = 1

   'alarm reset'
   If Blinkcount > 10 Then
     Blinkcount = 0
     Buzzer = 0
   End If

 End If


 'calculate steering'
 Rockersq = Rocker_zero - Ad_rocker



 'Steering depends  on speed'
 Buf1 = Drive_sum / 20000                                   '6000
 If Buf1 < 0 Then
    Buf1 = Buf1 * -1
 End If

 If Buf1 < 1 Then Buf1 = 1


 Rockersq = Rockersq / Buf1

 'some safety lines, limits steering
 'drivesum 55000  max = +- 5

 Buf1 = Drive_sum / 2000
 If Buf1 < 0 Then Buf1 = Buf1 * -1

 If Buf1 > 22 Then Buf1 = 22


 Buf1 = 27 - Buf1
 Buf2 = Buf1 * -1



 If Rockersq > Buf1 Then Rockersq = Buf1
 If Rockersq < Buf2 Then Rockersq = Buf2


 Steeringsignal = Rockersq


'overcurren situation?
 If Pind.2 = 0 Then
   Limitcurr = 125
 End If

 If Pind.2 = 1 Then
  If Limitcurr < 30000 Then
    Limitcurr = Limitcurr + 1
  End If
  Buzzer = 1
 End If



 Drivespeed = Drivespeed * 125
 Drivespeed = Drivespeed / Limitcurr


 'set speed and steering
 Drive_a = Drivespeed - Steeringsignal
 Drive_b = Drivespeed + Steeringsignal


 'need some seconds to compensate the gyro temp-drift
 If Mmode = _warmup Then
    Drive_a = 0
    Drive_b = 0
    Drivespeed = 0
    Anglecorrection = 0
    'Overspeed = 0
    Drive_sum = 0
    Total_adxl_gyro = 0
    Tilt_angle = 0

    If Loopct > 200 Then
      Mmode = _standby
    End If
 End If

    If Mmode = _standby Then
        Drive_a = 0
        Drive_b = 0
        Drivespeed = 0
        Anglecorrection = 0
        'Overspeed = 0
        Drive_sum = 0
        Total_adxl_gyro = 0
        Tilt_angle = 0
        Buf2 = Ad_adxl - Adxl_zero
        Buf2 = Buf2 + Adxl_offset
        Timeout = 0

        'stand on platform
        If Ad_swi > Sw_down Then
             Mmode = _run
        End If

    End If

    If Mmode = _run Then

        'check platform buttons
        If Ad_swi > Sw_down Then
          'Person on board,
          Timeout = 0
        End If

        If Ad_swi < Sw_down Then
          'no Person on board!, check savespeed
          If Drive_sum < 0 Then
            If Drive_sum < Msafespeed Then
              Buzzer = 1
              Timeout = Timeout + 1
            End If
          End If

          If Drive_sum > 0 Then
            If Drive_sum > Safespeed Then
              Buzzer = 1
              Timeout = Timeout + 1
            End If
          End If

          If Timeout > Critical Then
              Mmode = _down
          End If

        End If


   End If



   If Mmode = _down Then

     For Buf1 = 1 To 255

        If Drive_a > 0 Then
          Drive_a = Drive_a - 1
        End If

        If Drive_a < 0 Then
           Drive_a = Drive_a + 1
         End If

        If Drive_b > 0 Then
          Drive_b = Drive_b - 1
        End If

        If Drive_b < 0 Then
          Drive_b = Drive_b + 1
        End If

        Waitms 2
        Gosub Set_pwm

     Next Buf1

     Mmode = _standby



   End If



   Return
End Sub


'-----------------------------------------------------------------------'


Sub Ini
' do some assembler inits
  $asm
  ldi r16,0
  Out Porta , R16
  Out Ddra , R16
  ldi r16,0
  Out Portc , R16
  ldi r16,255
  Out Ddrc , R16
  ldi r16,$34
  Out Portd , R16
  ldi r16,251
  Out Ddrd , R16
  ldi r16,$b1                                               'b1=8bit ,b2=9bit,b3=10bit
  Out Tccr1a , R16
  ldi r16,$01                                               ' 01 no prescaler!
  Out Tccr1b , R16
  ldi r16,0
  Out Tcnt1h , R16
  Out Tcnt1l , R16
  Out Icr1h , R16
  Out Icr1l , R16
  Out Ocr1ah , R16
  ldi r16,$ff
  Out Ocr1al , R16
  ldi r16,0
  ldi r16,0
  Out Ocr1bl , R16
  Out Assr , R16
  Out Tccr2 , R16
  Out Tcnt2 , R16
  Out Ocr2 , R16
  $end Asm


Total_adxl_gyro = 0
Average_gyro = 0
Average_batt = Battok * Total_looptime

Drivea = 0
Driveb = 0
Tilt_angle = 0
Drive_a = 0
Drive_b = 0
Drivespeed = 0
Steeringsignal = 0
Anglecorrection = 0
Angle_rate = 0
'Voltage = 0
Balance_moment = 0
'Overspeed = 0

Mmode = _standby
'Overspeed_flag = 0
Drive_sum = 0
Ad_rocker = 0
Tcount = 0
Steeringsignal = 0

Timeout = 0
Delta = 0
Loopct = 0
Limitcurr = 100
Blinkcount = 0

Adxl_offset = 0


End Sub

'----------------------------------------------------'

Tinter:
    Disable Interrupts
    Gosub Get_tilt_angle
    Gosub Getspeedlimit
    Gosub Algo
    Gosub Process
    Gosub Set_pwm
    Timer0 = 100
    Enable Interrupts
    Return
'----------------------------------------------------'


Sub Err_value
  S3:
  For Buf = 1 To Errno
    Set Led3
    Waitms 150
    Reset Led3
    Waitms 150
  Next Buf

  Wait 2
  Goto S3
End Sub

'----------------------------------------------------'



Sub Get_tilt_angle
    Ad_gyro = Getadc(5)
    Ad_gyro = 1024 - Ad_gyro

    Ad_adxl = Getadc(3)
    Ad_batt = Getadc(0)
    Ad_rocker = Getadc(6)
    Ad_swi = 1024 - Getadc(7)


    Buf = Total_adxl_gyro / Total_looptime
    Total_adxl_gyro = Total_adxl_gyro - Buf

    ' ADXL part
    Buf = Ad_adxl - Adxl_zero


	Buf = Buf / 300   'add on 2011-04-18
	Buf = Buf * 174   'add on 2011-04-18


    Buf = Buf + Adxl_offset
    Total_adxl_gyro = Total_adxl_gyro + Buf

    ' Gyro part
    Buf1 = Average_gyro / Total_looptime
    Average_gyro = Average_gyro - Buf1

    Average_gyro = Average_gyro + Ad_gyro
    Buf1 = Average_gyro / Total_looptime10

    ' calculate the Angle Rate
    Buf = Ad_gyro * 10
    Buf1 = Buf1 - Buf
    Buf1 = Buf1 * 35
    Buf1 = Buf1 / 100
    Angle_rate = Buf1

    ' calculate the Tilt Angle
    Total_adxl_gyro = Total_adxl_gyro + Angle_rate
    Tilt_angle = Total_adxl_gyro / Total_looptime10

    'calculate batt_voltage
    Buf1 = Average_batt / Total_looptime
    Average_batt = Average_batt - Buf1
    Average_batt = Average_batt + Ad_batt


    Loopct = Loopct + 1


    Return
End Sub

  End
