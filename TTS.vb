Public Class frmTTS
    Const KILBURN = 23.9
    Const OXFORD = 67.73
    Const LEEDS = 53.89
    Const PRESTON = 78
    Const BRIXTON = 56
    Const TAX = 19.75

    Const STANDARD = 0
    Const FIRSTCLASS = 10
    Const VIP = 20

    Dim totalCost As Double

    Dim num1, num2 As Decimal
    Dim operan As Integer
    Dim operandSelected As Boolean = False

    Private Sub frmTTS_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ComboBoxDestination.Items.Add("Kilburn")
        ComboBoxDestination.Items.Add("Oxford")
        ComboBoxDestination.Items.Add("Leeds")
        ComboBoxDestination.Items.Add("Preston")
        ComboBoxDestination.Items.Add("Brixton")
    End Sub

    Private Sub ButtonZero_Click(sender As Object, e As EventArgs) Handles ButtonZero.Click
        If LabelDisplay.Text <> "0" Then
            LabelDisplay.Text += "0"
        Else
            LabelDisplay.Text = "0"
        End If
    End Sub

    Private Sub ButtonOne_Click(sender As Object, e As EventArgs) Handles ButtonOne.Click
        If LabelDisplay.Text <> "1" Then
            LabelDisplay.Text += "1"
        Else
            LabelDisplay.Text = "1"
        End If
    End Sub

    Private Sub ButtonTwo_Click(sender As Object, e As EventArgs) Handles ButtonTwo.Click
        If LabelDisplay.Text <> "2" Then
            LabelDisplay.Text += "2"
        Else
            LabelDisplay.Text = "2"
        End If
    End Sub

    Private Sub ButtonThree_Click(sender As Object, e As EventArgs) Handles ButtonThree.Click
        If LabelDisplay.Text <> "3" Then
            LabelDisplay.Text += "3"
        Else
            LabelDisplay.Text = "3"
        End If
    End Sub

    Private Sub ButtonFour_Click(sender As Object, e As EventArgs) Handles ButtonFour.Click
        If LabelDisplay.Text <> "4" Then
            LabelDisplay.Text += "4"
        Else
            LabelDisplay.Text = "4"
        End If
    End Sub

    Private Sub ButtonFive_Click(sender As Object, e As EventArgs) Handles ButtonFive.Click
        If LabelDisplay.Text <> "5" Then
            LabelDisplay.Text += "5"
        Else
            LabelDisplay.Text = "5"
        End If
    End Sub

    Private Sub ButtonSix_Click(sender As Object, e As EventArgs) Handles ButtonSix.Click
        If LabelDisplay.Text <> "6" Then
            LabelDisplay.Text += "6"
        Else
            LabelDisplay.Text = "6"
        End If
    End Sub

    Private Sub ButtonSeven_Click(sender As Object, e As EventArgs) Handles ButtonSeven.Click
        If LabelDisplay.Text <> "7" Then
            LabelDisplay.Text += "7"
        Else
            LabelDisplay.Text = "7"
        End If
    End Sub

    Private Sub ButtonEight_Click(sender As Object, e As EventArgs) Handles ButtonEight.Click
        If LabelDisplay.Text <> "8" Then
            LabelDisplay.Text += "8"
        Else
            LabelDisplay.Text = "8"
        End If
    End Sub

    Private Sub ButtonNine_Click(sender As Object, e As EventArgs) Handles ButtonNine.Click
        If LabelDisplay.Text <> "9" Then
            LabelDisplay.Text += "9"
        Else
            LabelDisplay.Text = "9"
        End If
    End Sub

    Private Sub ButtonPlus_Click(sender As Object, e As EventArgs) Handles ButtonPlus.Click
        num1 = LabelDisplay.Text
        LabelDisplay.Text = "0"
        operandSelected = True
        operan = 1
    End Sub

    Private Sub ButtonSub_Click(sender As Object, e As EventArgs) Handles ButtonSub.Click
        num1 = LabelDisplay.Text
        LabelDisplay.Text = "0"
        operandSelected = True
        operan = 2
    End Sub

    Private Sub ButtonMul_Click(sender As Object, e As EventArgs) Handles ButtonMul.Click
        num1 = LabelDisplay.Text
        LabelDisplay.Text = "0"
        operandSelected = True
        operan = 3
    End Sub

    Private Sub ButtonDiv_Click(sender As Object, e As EventArgs) Handles ButtonDiv.Click
        num1 = LabelDisplay.Text
        LabelDisplay.Text = "0"
        operandSelected = True
        operan = 4
    End Sub

    Private Sub ButtonDot_Click(sender As Object, e As EventArgs) Handles ButtonDot.Click
        If InStr(LabelDisplay.Text, "0") = 0 Then
            LabelDisplay.Text = LabelDisplay.Text + "."
        End If
    End Sub

    Private Sub ButtonMod_Click(sender As Object, e As EventArgs) Handles ButtonMod.Click
        num1 = LabelDisplay.Text
        LabelDisplay.Text = "0"
        operandSelected = True
        operan = 5
    End Sub

    Private Sub ButtonEqual_Click(sender As Object, e As EventArgs) Handles ButtonEqual.Click
        If operandSelected = True Then
            num2 = LabelDisplay.Text
            If operan = 1 Then
                LabelDisplay.Text = num1 + num2
            ElseIf operan = 2 Then
                LabelDisplay.Text = num1 - num2
            ElseIf operan = 3 Then
                LabelDisplay.Text = num1 * num2
            ElseIf operan = 4 Then
                If num2 = "0" Then
                    LabelDisplay.Text = "Error"
                Else
                    LabelDisplay.Text = num1 / num2
                End If
            End If
        ElseIf operan = 5 Then
            LabelDisplay.Text = num1 Mod num2
            operandSelected = False
        End If
    End Sub

    Private Sub ButtonClear_Click(sender As Object, e As EventArgs) Handles ButtonClear.Click
        LabelDisplay.Text = ""
        LabelDisplay.Text = "0"
    End Sub

    Private Sub RadioButtonUseCalculator_CheckedChanged(sender As Object, e As EventArgs) Handles RadioButtonUseCalculator.CheckedChanged
        If RadioButtonUseCalculator.Checked = True Then
            ButtonZero.Enabled = True
            ButtonOne.Enabled = True
            ButtonTwo.Enabled = True
            ButtonThree.Enabled = True
            ButtonFour.Enabled = True
            ButtonFive.Enabled = True
            ButtonSix.Enabled = True
            ButtonSeven.Enabled = True
            ButtonEight.Enabled = True
            ButtonNine.Enabled = True
            ButtonPlus.Enabled = True
            ButtonSub.Enabled = True
            ButtonMul.Enabled = True
            ButtonDiv.Enabled = True
            ButtonDot.Enabled = True
            ButtonMod.Enabled = True
            ButtonClear.Enabled = True
            ButtonEqual.Enabled = True
        End If
    End Sub

    Private Sub ButtonTotal_Click(sender As Object, e As EventArgs) Handles ButtonTotal.Click


        If (ComboBoxDestination.Text = "Kilburn") And (RadioButtonClassStandard.Checked Or RadioButtonClassFirstClass.Checked Or RadioButtonClassVIP.Checked) _
            And (RadioButtonTicketTypeReturn.Checked Or RadioButtonTicketTypeSingle.Checked) And (RadioButtonAdultYes.Checked Or RadioButtonChildYes.Checked) Then

            If RadioButtonClassStandard.Checked = True Then
                LabelClass.Text = RadioButtonClassStandard.Text


                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text


                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(KILBURN, "adult", "standard")


                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(KILBURN, "child", "standard")
                    End If


                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text


                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(KILBURN, "adult", "standard")


                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(KILBURN, "child", "standard")
                    End If
                End If

            ElseIf RadioButtonClassFirstClass.Checked = True Then
                LabelClass.Text = RadioButtonClassFirstClass.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(KILBURN, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(KILBURN, "child", "firstclass")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(KILBURN, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(KILBURN, "child", "firstclass")
                    End If
                End If

            ElseIf RadioButtonClassVIP.Checked = True Then

                Dim VIPClass As Byte = 40

                LabelClass.Text = RadioButtonClassVIP.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(KILBURN, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(KILBURN, "child", "vip")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(KILBURN, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(KILBURN, "child", "vip")
                    End If
                End If
            End If
        End If

        If (ComboBoxDestination.Text = "Oxford") And (RadioButtonClassStandard.Checked Or RadioButtonClassFirstClass.Checked Or RadioButtonClassVIP.Checked) _
            And (RadioButtonTicketTypeReturn.Checked Or RadioButtonTicketTypeSingle.Checked) And (RadioButtonAdultYes.Checked Or RadioButtonChildYes.Checked) Then

            If RadioButtonClassStandard.Checked = True Then
                LabelClass.Text = RadioButtonClassStandard.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(OXFORD, "adult", "standard")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(OXFORD, "child", "standard")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(OXFORD, "adult", "standard")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(OXFORD, "child", "standard")
                    End If
                End If

            ElseIf RadioButtonClassFirstClass.Checked = True Then
                LabelClass.Text = RadioButtonClassFirstClass.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(OXFORD, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(OXFORD, "child", "firstclass")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(OXFORD, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(OXFORD, "child", "firstclass")
                    End If
                End If

            ElseIf RadioButtonClassVIP.Checked = True Then

                Dim VIPClass As Byte = 40

                LabelClass.Text = RadioButtonClassVIP.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(OXFORD, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(OXFORD, "child", "vip")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(OXFORD, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(OXFORD, "child", "vip")
                    End If
                End If
            End If
        End If

        If (ComboBoxDestination.Text = "Leeds") And (RadioButtonClassStandard.Checked Or RadioButtonClassFirstClass.Checked Or RadioButtonClassVIP.Checked) _
            And (RadioButtonTicketTypeReturn.Checked Or RadioButtonTicketTypeSingle.Checked) And (RadioButtonAdultYes.Checked Or RadioButtonChildYes.Checked) Then

            If RadioButtonClassStandard.Checked = True Then
                LabelClass.Text = RadioButtonClassStandard.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(LEEDS, "adult", "standard")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(LEEDS, "child", "standard")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(LEEDS, "adult", "standard")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(LEEDS, "child", "standard")
                    End If
                End If

            ElseIf RadioButtonClassFirstClass.Checked = True Then
                LabelClass.Text = RadioButtonClassFirstClass.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(LEEDS, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(LEEDS, "child", "firstclass")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(LEEDS, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(LEEDS, "child", "firstclass")
                    End If
                End If

            ElseIf RadioButtonClassVIP.Checked = True Then

                Dim VIPClass As Byte = 40

                LabelClass.Text = RadioButtonClassVIP.Text


                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text


                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(LEEDS, "adult", "vip")


                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(LEEDS, "child", "vip")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(LEEDS, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(LEEDS, "child", "vip")
                    End If
                End If
            End If
        End If

        If (ComboBoxDestination.Text = "Preston") And (RadioButtonClassStandard.Checked Or RadioButtonClassFirstClass.Checked Or RadioButtonClassVIP.Checked) _
            And (RadioButtonTicketTypeReturn.Checked Or RadioButtonTicketTypeSingle.Checked) And (RadioButtonAdultYes.Checked Or RadioButtonChildYes.Checked) Then

            If RadioButtonClassStandard.Checked = True Then
                LabelClass.Text = RadioButtonClassStandard.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(PRESTON, "adult", "standard")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(PRESTON, "child", "standard")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(PRESTON, "adult", "standard")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(PRESTON, "child", "standard")
                    End If
                End If

            ElseIf RadioButtonClassFirstClass.Checked = True Then
                LabelClass.Text = RadioButtonClassFirstClass.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(PRESTON, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(PRESTON, "child", "firstclass")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(PRESTON, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(PRESTON, "child", "firstclass")
                    End If
                End If

            ElseIf RadioButtonClassVIP.Checked = True Then

                Dim VIPClass As Byte = 40

                LabelClass.Text = RadioButtonClassVIP.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(PRESTON, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(PRESTON, "child", "vip")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(PRESTON, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(PRESTON, "child", "vip")
                    End If
                End If
            End If
        End If

        If (ComboBoxDestination.Text = "Brixton") And (RadioButtonClassStandard.Checked Or RadioButtonClassFirstClass.Checked Or RadioButtonClassVIP.Checked) _
            And (RadioButtonTicketTypeReturn.Checked Or RadioButtonTicketTypeSingle.Checked) And (RadioButtonAdultYes.Checked Or RadioButtonChildYes.Checked) Then

            If RadioButtonClassStandard.Checked = True Then
                LabelClass.Text = RadioButtonClassStandard.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(BRIXTON, "adult", "standard")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(BRIXTON, "child", "standard")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(BRIXTON, "adult", "standard")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(BRIXTON, "child", "standard")
                    End If
                End If

            ElseIf RadioButtonClassFirstClass.Checked = True Then
                LabelClass.Text = RadioButtonClassFirstClass.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(BRIXTON, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(BRIXTON, "child", "firstclass")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(BRIXTON, "adult", "firstclass")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(BRIXTON, "child", "firstclass")
                    End If
                End If

            ElseIf RadioButtonClassVIP.Checked = True Then

                Dim VIPClass As Byte = 40

                LabelClass.Text = RadioButtonClassVIP.Text

                If RadioButtonTicketTypeSingle.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeSingle.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(BRIXTON, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(BRIXTON, "child", "vip")
                    End If

                ElseIf RadioButtonTicketTypeReturn.Checked = True Then
                    LabelTicket.Text = RadioButtonTicketTypeReturn.Text

                    If RadioButtonAdultYes.Checked = True Then
                        Ticket(BRIXTON, "adult", "vip")

                    ElseIf RadioButtonChildYes.Checked = True Then
                        Ticket(BRIXTON, "child", "vip")
                    End If
                End If
            End If
        End If

        RefNoTimeDateRoute()
    End Sub

    Private Sub RefNoTimeDateRoute()
        Randomize()
        LabelRefNo.Text = (Str(Int(Rnd() * 100000)))
        LabelTime.Text = TimeOfDay
        LabelDate.Text = Today()
        LabelRoute.Text = "ANY"
    End Sub

    Private Sub Ticket(ByVal whereDestination As Double, ByVal isAdultOrChild As String, ByVal whatClass As String)

        Dim target As Double = whereDestination

        If whatClass = "standard" Then
            If isAdultOrChild = "adult" Then
                LabelAdult.Text = RadioButtonAdultYes.Text
                LabelChild.Text = "-"
                LabelFrom.Text = "London"
                LabelTo.Text = ComboBoxDestination.Text
                totalCost = (target * TAX) / 100
                TextBoxSubTotal.Text = target
                TextBoxTax.Text = totalCost
                TextBoxTotal.Text = totalCost + target
                LabelPrice.Text = (totalCost + target + STANDARD).ToString("N2")
            ElseIf isAdultOrChild = "child" Then
                LabelAdult.Text = "-"
                LabelChild.Text = RadioButtonChildYes.Text
                LabelFrom.Text = "London"
                LabelTo.Text = ComboBoxDestination.Text
                totalCost = (target * TAX) / 100
                TextBoxSubTotal.Text = target
                TextBoxTax.Text = totalCost
                TextBoxTotal.Text = totalCost + target
                LabelPrice.Text = ((totalCost + target + STANDARD) - 20).ToString("N2")
            End If
        ElseIf whatClass = "firstclass" Then
            If isAdultOrChild = "adult" Then
                LabelAdult.Text = RadioButtonAdultYes.Text
                LabelChild.Text = "-"
                LabelFrom.Text = "London"
                LabelTo.Text = ComboBoxDestination.Text
                totalCost = (target * TAX) / 100
                TextBoxSubTotal.Text = target
                TextBoxTax.Text = totalCost
                TextBoxTotal.Text = totalCost + target
                LabelPrice.Text = (totalCost + target + FIRSTCLASS).ToString("N2")
            ElseIf isAdultOrChild = "child" Then
                LabelAdult.Text = "-"
                LabelChild.Text = RadioButtonChildYes.Text
                LabelFrom.Text = "London"
                LabelTo.Text = ComboBoxDestination.Text
                totalCost = (target * TAX) / 100
                TextBoxSubTotal.Text = target
                TextBoxTax.Text = totalCost
                TextBoxTotal.Text = totalCost + target
                LabelPrice.Text = ((totalCost + target + FIRSTCLASS) - 20).ToString("N2")
            End If
        ElseIf whatClass = "vip" Then
            If isAdultOrChild = "adult" Then
                LabelAdult.Text = RadioButtonAdultYes.Text
                LabelChild.Text = "-"
                LabelFrom.Text = "London"
                LabelTo.Text = ComboBoxDestination.Text
                totalCost = (target * TAX) / 100
                TextBoxSubTotal.Text = target
                TextBoxTax.Text = totalCost
                TextBoxTotal.Text = totalCost + target
                LabelPrice.Text = (totalCost + target + VIP).ToString("N2")
            ElseIf isAdultOrChild = "child" Then
                LabelAdult.Text = "-"
                LabelChild.Text = RadioButtonChildYes.Text
                LabelFrom.Text = "London"
                LabelTo.Text = ComboBoxDestination.Text
                totalCost = (target * TAX) / 100
                TextBoxSubTotal.Text = target
                TextBoxTax.Text = totalCost
                TextBoxTotal.Text = totalCost + target
                LabelPrice.Text = ((totalCost + target + VIP) - 20).ToString("N2")
            End If
        End If
    End Sub

    Private Sub ButtonExit_Click(sender As Object, e As EventArgs) Handles ButtonExite.Click
        Application.Exit()
    End Sub
End Class
