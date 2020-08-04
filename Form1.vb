Public Class Form1
    Public lens As Integer
    Public cek As Boolean
    Public A As String
    Public B As String
    Public C As String
    Public satu As String
    Public nol As String
    Public cekA As Boolean
    Public cekB As Boolean
    Public cekC As Boolean
    Public cekNol As Boolean
    Public cekSatu As Boolean
    Dim check As Boolean = False
    Dim checkSatu As Boolean = True
    Dim checkNol As Boolean = True
    Public tops As Integer
    Public element(100) As Char
    Public result As String
    Dim myStack As Stack = New Stack
    Public Function empty()
        If (tops < 0) Then
            Return True
        Else
            Return False
        End If
    End Function
    Public Function full()
        If (tops > 100) Then
            Return True
        Else
            Return False
        End If
    End Function
    Public Function pop(ByRef x As Char)
        If (empty()) Then
            TextBox2.Text = "Stack is empty"
            Return False
        Else
            x = element(tops)
            tops -= 1
            Return True
        End If
    End Function
    Public Function push(ByVal x As Char)
        If (full()) Then
            TextBox2.Text = "Stack is full"
            Return False
        Else
            tops += 1
            element(tops) = x
            Return True
        End If
    End Function
    Public Function retrieve()
        Dim x As Char
        If (empty()) Then
            TextBox2.Text = "Stack is empty"
            Return ""
        Else
            x = element(tops)
            Return x
        End If
    End Function
    Public Function valueOperator(ByVal x As Char)
        If x = "ㄱ" Then
            Return 5
        ElseIf x = "^" Then
            Return 4
        ElseIf x = "v" Then
            Return 3
        ElseIf x = "-" Then
            Return 2
        ElseIf x = "~" Then
            Return 1
        Else
            Return 0
        End If
    End Function
    Public Function postFix(ByVal x As String)
        Dim postfixx As String
        Dim popped As Char
        Dim i, j As Integer

        postfixx = ""
        j = 1
        For i = 1 To lens
            x = Mid(TextBox1.Text, i, j)
            If (x = "A" Or x = "B" Or x = "C" Or x = "0" Or x = "1") Then
                postfixx += x
            ElseIf (x = "^" Or x = "v" Or x = "-" Or x = "ㄱ" Or x = "~") Then
                While empty() <> True And valueOperator(retrieve()) >= valueOperator(x) And retrieve() <> "("
                    pop(popped)
                    postfixx += popped
                End While
                push(x)
            ElseIf x = "(" Then
                push(x)
            ElseIf x = ")" Then
                While retrieve() <> "("
                    postfixx += retrieve()
                    pop(popped)
                End While
                pop(popped)
            End If
        Next

        While empty() = False
            pop(popped)
            postfixx += popped
        End While

        Return postfixx
    End Function
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        TextBox1.Text += "A"
    End Sub
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        TextBox1.Text += "B"
    End Sub
    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        TextBox1.Text += "C"
    End Sub
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        TextBox1.Text += "^"
    End Sub
    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        TextBox1.Text += "0"
    End Sub
    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        TextBox1.Text += "1"
    End Sub
    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        TextBox1.Text += "ㄱ"
    End Sub
    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        TextBox1.Text += "v"
    End Sub
    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click
        TextBox1.Text += "("
    End Sub
    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click
        TextBox1.Text += ")"
    End Sub
    Private Sub Button13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button13.Click
        TextBox1.Text += "-"
    End Sub
    Private Sub Button14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button14.Click
        TextBox1.Text += "~"
    End Sub
    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        TextBox1.Clear()
        TextBox2.Clear()
        TextBox3.Clear()
        DataGridView1.Rows.Clear()
    End Sub
    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        If TextBox1.Text < " " Then
            TextBox1.Text = Mid(TextBox1.Text, 1, Len(TextBox1.Text) - 1 + 1)
        Else
            TextBox1.Text = Mid(TextBox1.Text, 1, Len(TextBox1.Text) - 1)
        End If
    End Sub
    Public Function Calculate(ByVal result As String)
        Dim hasil As String = ""
        Dim isi As String
        Dim var1 As String
        Dim var2 As String
        Dim x As String


        For i = 1 To lens
            hasil = ""
            isi = Mid(result, i, 1)
            If (isi = "A" Or isi = "B" Or isi = "C" Or isi = "0" Or isi = "1") Then
                If (cekA = True And cekB = False And cekC = False) Then
                    If (cekNol = True And check = True) Then
                        nol = "00"
                        hasil = nol
                        check = False
                    ElseIf (cekSatu = True And check = True) Then
                        satu = "11"
                        hasil = satu
                        check = False
                    Else
                        A = "10"
                        hasil = A
                        check = True
                    End If
                ElseIf (cekA = False And cekB = True And cekC = False) Then
                    If (cekNol = True And check = True) Then
                        nol = "00"
                        hasil = nol
                        check = False
                    ElseIf (cekSatu = True And check = True) Then
                        satu = "11"
                        hasil = satu
                        check = False
                    Else
                        B = "10"
                        hasil = B
                        check = True
                    End If
                ElseIf (cekA = False And cekB = False And cekC = True) Then
                    If (cekNol = True And check = True) Then
                        nol = "00"
                        hasil = nol
                        check = False
                    ElseIf (cekSatu = True And check = True) Then
                        satu = "11"
                        hasil = satu
                        check = False
                    Else
                        C = "10"
                        hasil = C
                        check = True
                    End If
                ElseIf (cek = False And cekA = True And cekB = True) Then
                    If (cekNol = True And check = True) Then
                        nol = "0000"
                        hasil = nol
                        check = False
                    ElseIf (cekSatu = True And check = True) Then
                        satu = "1111"
                        hasil = satu
                        check = False
                    End If
                    If (isi = "A") Then
                        A = "1100"
                        hasil = A
                        check = True
                    ElseIf (isi = "B") Then
                        B = "1010"
                        hasil = B
                        check = True
                    End If
                ElseIf (cek = False And cekC = True And cekB = True) Then
                    If (cekNol = True And check = True) Then
                        nol = "0000"
                        hasil = nol
                        check = False
                    ElseIf (cekSatu = True And check = True) Then
                        satu = "1111"
                        hasil = satu
                        check = False
                    End If
                    If (isi = "C") Then
                        C = "1010"
                        hasil = C
                    ElseIf (isi = "B") Then
                        B = "1100"
                        hasil = B
                    End If
                ElseIf (cek = False And cekA = True And cekC = True) Then
                    If (cekNol = True And check = True) Then
                        nol = "0000"
                        hasil = nol
                        check = False
                    ElseIf (cekSatu = True And check = True) Then
                        satu = "1111"
                        hasil = satu
                        check = False
                    End If
                    If (isi = "C") Then
                        C = "1010"
                        hasil = C
                    ElseIf (isi = "A") Then
                        A = "1100"
                        hasil = A
                    End If
                ElseIf (cek = True And cekNol = True And cekSatu = True) Then
                    If ((isi = "1" And check = True And checkSatu = False) And checkNol = True) Then
                        satu = "11111111"
                        hasil = satu
                        check = False
                        checkSatu = True
                    ElseIf (isi = "0" And check = True) Then
                        nol = "00000000"
                        hasil = nol
                        checkNol = True
                    End If
                    If (isi = "A") Then
                        A = "11110000"
                        hasil = A
                        check = True
                        checkSatu = False
                        checkNol = False
                    ElseIf (isi = "B") Then
                        B = "11001100"
                        hasil = B
                        check = True
                        checkSatu = False
                        checkNol = False
                    ElseIf (isi = "C") Then
                        C = "10101010"
                        hasil = C
                        check = True
                        checkSatu = False
                        checkNol = False
                    End If
                ElseIf (cek = True) Then
                    If (isi = "1" And check = True) Then
                        satu = "11111111"
                        hasil = satu
                        check = False
                    ElseIf (isi = "0" And check = True) Then
                        nol = "00000000"
                        hasil = nol
                        check = False
                    End If
                    If (isi = "A") Then
                        A = "11110000"
                        hasil = A
                        check = True
                    ElseIf (isi = "B") Then
                        B = "11001100"
                        hasil = B
                        check = True
                    ElseIf (isi = "C") Then
                        C = "10101010"
                        hasil = C
                        check = True
                    End If
                ElseIf (cekNol = True) Then
                    nol = "0"
                    hasil = nol
                ElseIf (cekSatu = True) Then
                    satu = "1"
                    hasil = satu
                End If
                myStack.Push(hasil)

                'negasi
            ElseIf (isi = "ㄱ") Then
                x = myStack.Pop() '1010
                Dim k As Integer
                Dim temp As String = ""
                Dim var As Char
                Dim count As Integer
                count = Len(x)

                For k = 1 To count
                    var = Mid(x, k, 1)
                    If (var = "1") Then
                        var = "0"
                    ElseIf (var = "0") Then
                        var = "1"
                    End If
                    temp += var
                Next
                myStack.Push(temp)

                'dan
            ElseIf (isi = "^") Then
                var1 = myStack.Pop()
                var2 = myStack.Pop()
                Dim p1 As Integer
                p1 = Len(var1)

                For j = 1 To p1
                    If (Mid(var1, j, 1) = "1" And Mid(var2, j, 1) = "1") Then
                        hasil += "1"
                    Else
                        hasil += "0"
                    End If
                Next
                myStack.Push(hasil)

                'atau
            ElseIf (isi = "v") Then
                hasil = ""
                var1 = myStack.Pop()
                var2 = myStack.Pop()
                Dim p1 As Integer
                p1 = Len(var1)

                For j = 1 To p1
                    If (Mid(var1, j, 1) = "0" And Mid(var2, j, 1) = "0") Then
                        hasil += "0"
                    Else
                        hasil += "1"
                    End If
                Next
                myStack.Push(hasil)

                'implikasi
            ElseIf (isi = "-") Then
                hasil = ""
                var1 = myStack.Pop()
                var2 = myStack.Pop()
                Dim p1 As Integer
                p1 = Len(var1)

                For j = 1 To p1
                    If (Mid(var2, j, 1) = "1" And Mid(var1, j, 1) = "0") Then
                        hasil += "0"
                    Else
                        hasil += "1"
                    End If
                Next
                myStack.Push(hasil)

                'biimplikasi
            ElseIf (isi = "~") Then
                hasil = ""
                var1 = myStack.Pop()
                var2 = myStack.Pop()
                Dim p1 As Integer
                p1 = Len(var1)

                For j = 1 To p1
                    If (Mid(var1, j, 1) = Mid(var2, j, 1)) Then
                        hasil += "1"
                    Else
                        hasil += "0"
                    End If
                Next
                myStack.Push(hasil)
            End If

        Next

        Return myStack.Pop()
    End Function
    Private Sub Button15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button15.Click
        Dim poo As Integer
        For poo = 1 To 1
            cek = False
            cekA = False
            cekB = False
            cekC = False
            cekNol = False
            cekSatu = False
            tops = 1
            Dim str As String = ""
            lens = Len(TextBox1.Text)


            Dim jmlhProses As Integer
            jmlhProses = (lens / 3) + 1

            Dim m As Integer
            Dim countKurungBuka As Integer
            Dim countKurungTutup As Integer
            Dim indexAwal(jmlhProses) As Integer
            Dim indexAkhir(jmlhProses) As Integer
            Dim co As Integer = 0
            Dim tmp As String


            'For m = 1 To lens
            '    tmp = Mid(TextBox1.Text, m, 1)
            '    If (tmp = "(") Then
            '        indexAwal(co) = m
            '    ElseIf (tmp = ")") Then
            '        indexAkhir(co) = m
            '    End If
            '    co += 1
            'Next
            ''Dim coco As Integer
            'For coco = 1 To co
            '    If (indexAkhir(coco) < indexAwal(coco)) Then
            '        'MsgBox("Tanda kurung terbalik")
            '        TextBox2.Text = "Tanda kurung terbalik"
            '    End If
            '    coco += 1
            'Next

            For m = 1 To lens
                tmp = Mid(TextBox1.Text, m, 1)
                If (tmp = "(") Then
                    countKurungBuka += 1
                ElseIf (tmp = ")") Then
                    countKurungTutup += 1
                End If
            Next
            Dim eror As String
            If (countKurungBuka <> countKurungTutup) Then
                If (countKurungBuka < countKurungTutup) Then
                    eror = "Jumlah kurung buka lebih sedikit dari jumlah kurung tutup"
                    TextBox2.Text = eror
                ElseIf (countKurungBuka > countKurungTutup) Then
                    eror = "Jumlah kurung tutup lebih sedikit dari jumlah kurung buka"
                    TextBox2.Text = eror
                End If
                Exit For
            End If

            Dim i, j As Integer
            j = 1
            For i = 1 To lens
                str = Mid(TextBox1.Text, i, j)
                If (str = "C") Then
                    cekC = True
                ElseIf (str = "B") Then
                    cekB = True
                ElseIf (str = "A") Then
                    cekA = True
                ElseIf (str = "0") Then
                    cekNol = True
                ElseIf (str = "1") Then
                    cekSatu = True
                End If
            Next

            If (cekA = True And cekC = True And cekB = True) Then
                cek = True
            Else
                cek = False
            End If

            If (cek = True) Then
                A = "11110000"
                B = "11001100"
                C = "10101010"
                nol = "00000000"
                satu = "11111111"
            ElseIf (cek = False And cekA = True And cekB = False And cekC = False) Then
                A = "10"
            ElseIf (cek = False And cekA = False And cekB = True And cekC = False) Then
                B = "10"
            ElseIf (cek = False And cekA = False And cekB = False And cekC = True) Then
                C = "10"
            ElseIf (cek = False And cekC = False And cekA = True And cekB = True) Then
                A = "1100"
                B = "1010"
            ElseIf (cek = False And cekA = False And cekC = True And cekB = True) Then
                B = "1100"
                C = "1010"
            ElseIf (cek = False And cekB = False And cekA = True And cekC = True) Then
                A = "1100"
                C = "1010"
            ElseIf (cek = False And (cekA = True Or cekB = True Or cekC = True) And cekSatu = True) Then
                satu = "11"
            ElseIf (cek = False And (cekA = True Or cekB = True Or cekC = True) And cekNol = True) Then
                nol = "00"
            End If

            result = postFix(TextBox1.Text)
            'TextBox2.Text = Calculate(result)

            'TextBox3.Text = result
            lens = Len(result)

            If (cekA = True And cekB = True And cekC = True) Then
                DataGridView1.ColumnCount = 6
                DataGridView1.Columns(0).Name = "A"
                DataGridView1.Columns(1).Name = "B"
                DataGridView1.Columns(2).Name = "C"
                DataGridView1.Columns(3).Name = "0"
                DataGridView1.Columns(4).Name = "1"
                DataGridView1.Columns(5).Name = "Hasil"
                For i = 1 To 7
                    DataGridView1.Rows.Add()
                Next
                'A
                DataGridView1.Rows(0).Cells(0).Value = 1
                DataGridView1.Rows(1).Cells(0).Value = 1
                DataGridView1.Rows(2).Cells(0).Value = 1
                DataGridView1.Rows(3).Cells(0).Value = 1
                DataGridView1.Rows(4).Cells(0).Value = 0
                DataGridView1.Rows(5).Cells(0).Value = 0
                DataGridView1.Rows(6).Cells(0).Value = 0
                DataGridView1.Rows(7).Cells(0).Value = 0
                'B
                DataGridView1.Rows(0).Cells(1).Value = 1
                DataGridView1.Rows(1).Cells(1).Value = 1
                DataGridView1.Rows(2).Cells(1).Value = 0
                DataGridView1.Rows(3).Cells(1).Value = 0
                DataGridView1.Rows(4).Cells(1).Value = 1
                DataGridView1.Rows(5).Cells(1).Value = 1
                DataGridView1.Rows(6).Cells(1).Value = 0
                DataGridView1.Rows(7).Cells(1).Value = 0
                'C
                DataGridView1.Rows(0).Cells(2).Value = 1
                DataGridView1.Rows(1).Cells(2).Value = 0
                DataGridView1.Rows(2).Cells(2).Value = 1
                DataGridView1.Rows(3).Cells(2).Value = 0
                DataGridView1.Rows(4).Cells(2).Value = 1
                DataGridView1.Rows(5).Cells(2).Value = 0
                DataGridView1.Rows(6).Cells(2).Value = 1
                DataGridView1.Rows(7).Cells(2).Value = 0
                '0
                If (cekNol = True) Then
                    DataGridView1.Rows(0).Cells(3).Value = 0
                    DataGridView1.Rows(1).Cells(3).Value = 0
                    DataGridView1.Rows(2).Cells(3).Value = 0
                    DataGridView1.Rows(3).Cells(3).Value = 0
                    DataGridView1.Rows(4).Cells(3).Value = 0
                    DataGridView1.Rows(5).Cells(3).Value = 0
                    DataGridView1.Rows(6).Cells(3).Value = 0
                    DataGridView1.Rows(7).Cells(3).Value = 0
                Else
                    DataGridView1.Rows(0).Cells(3).Value = ""
                    DataGridView1.Rows(1).Cells(3).Value = ""
                    DataGridView1.Rows(2).Cells(3).Value = ""
                    DataGridView1.Rows(3).Cells(3).Value = ""
                    DataGridView1.Rows(4).Cells(3).Value = ""
                    DataGridView1.Rows(5).Cells(3).Value = ""
                    DataGridView1.Rows(6).Cells(3).Value = ""
                    DataGridView1.Rows(7).Cells(3).Value = ""
                End If
                '1
                If (cekSatu = True) Then
                    DataGridView1.Rows(0).Cells(4).Value = 1
                    DataGridView1.Rows(1).Cells(4).Value = 1
                    DataGridView1.Rows(2).Cells(4).Value = 1
                    DataGridView1.Rows(3).Cells(4).Value = 1
                    DataGridView1.Rows(4).Cells(4).Value = 1
                    DataGridView1.Rows(5).Cells(4).Value = 1
                    DataGridView1.Rows(6).Cells(4).Value = 1
                    DataGridView1.Rows(7).Cells(4).Value = 1
                Else
                    DataGridView1.Rows(0).Cells(4).Value = ""
                    DataGridView1.Rows(1).Cells(4).Value = ""
                    DataGridView1.Rows(2).Cells(4).Value = ""
                    DataGridView1.Rows(3).Cells(4).Value = ""
                    DataGridView1.Rows(4).Cells(4).Value = ""
                    DataGridView1.Rows(5).Cells(4).Value = ""
                    DataGridView1.Rows(6).Cells(4).Value = ""
                    DataGridView1.Rows(7).Cells(4).Value = ""
                End If
                'Hasil
                For i = 1 To 8
                    DataGridView1.Rows(i - 1).Cells(5).Value = Mid((Calculate(result)), i, 1)
                Next

            ElseIf ((cekA = True And cekB = True) Or (cekA = True And cekC = True) Or (cekC = True And cekB = True)) Then
                DataGridView1.ColumnCount = 6
                DataGridView1.Columns(0).Name = "A"
                DataGridView1.Columns(1).Name = "B"
                DataGridView1.Columns(2).Name = "C"
                DataGridView1.Columns(3).Name = "0"
                DataGridView1.Columns(4).Name = "1"
                DataGridView1.Columns(5).Name = "Hasil"
                For j = 1 To 3
                    DataGridView1.Rows.Add()
                Next

                If (cekA = True And cekB = True) Then
                    'A
                    DataGridView1.Rows(0).Cells(0).Value = 1
                    DataGridView1.Rows(1).Cells(0).Value = 1
                    DataGridView1.Rows(2).Cells(0).Value = 0
                    DataGridView1.Rows(3).Cells(0).Value = 0
                    'B
                    DataGridView1.Rows(0).Cells(1).Value = 1
                    DataGridView1.Rows(1).Cells(1).Value = 0
                    DataGridView1.Rows(2).Cells(1).Value = 1
                    DataGridView1.Rows(3).Cells(1).Value = 0
                    'C
                    DataGridView1.Rows(0).Cells(2).Value = ""
                    DataGridView1.Rows(1).Cells(2).Value = ""
                    DataGridView1.Rows(2).Cells(2).Value = ""
                    DataGridView1.Rows(3).Cells(2).Value = ""
                    '0
                    If (cekNol = True) Then
                        DataGridView1.Rows(0).Cells(3).Value = 0
                        DataGridView1.Rows(1).Cells(3).Value = 0
                        DataGridView1.Rows(2).Cells(3).Value = 0
                        DataGridView1.Rows(3).Cells(3).Value = 0
                    Else
                        DataGridView1.Rows(0).Cells(3).Value = ""
                        DataGridView1.Rows(1).Cells(3).Value = ""
                        DataGridView1.Rows(2).Cells(3).Value = ""
                        DataGridView1.Rows(3).Cells(3).Value = ""
                    End If
                    '1
                    If (cekSatu = True) Then
                        DataGridView1.Rows(0).Cells(4).Value = 1
                        DataGridView1.Rows(1).Cells(4).Value = 1
                        DataGridView1.Rows(2).Cells(4).Value = 1
                        DataGridView1.Rows(3).Cells(4).Value = 1
                    Else
                        DataGridView1.Rows(0).Cells(4).Value = ""
                        DataGridView1.Rows(1).Cells(4).Value = ""
                        DataGridView1.Rows(2).Cells(4).Value = ""
                        DataGridView1.Rows(3).Cells(4).Value = ""
                    End If
                    'Hasil
                    For i = 1 To 4
                        DataGridView1.Rows(i - 1).Cells(5).Value = Mid((Calculate(result)), i, 1)
                    Next
                ElseIf (cekA = True And cekC = True) Then
                    'A
                    DataGridView1.Rows(0).Cells(0).Value = 1
                    DataGridView1.Rows(1).Cells(0).Value = 1
                    DataGridView1.Rows(2).Cells(0).Value = 0
                    DataGridView1.Rows(3).Cells(0).Value = 0
                    'B
                    DataGridView1.Rows(0).Cells(1).Value = ""
                    DataGridView1.Rows(1).Cells(1).Value = ""
                    DataGridView1.Rows(2).Cells(1).Value = ""
                    DataGridView1.Rows(3).Cells(1).Value = ""
                    'C
                    DataGridView1.Rows(0).Cells(2).Value = 1
                    DataGridView1.Rows(1).Cells(2).Value = 0
                    DataGridView1.Rows(2).Cells(2).Value = 1
                    DataGridView1.Rows(3).Cells(2).Value = 0
                    '0
                    If (cekNol = True) Then
                        DataGridView1.Rows(0).Cells(3).Value = 0
                        DataGridView1.Rows(1).Cells(3).Value = 0
                        DataGridView1.Rows(2).Cells(3).Value = 0
                        DataGridView1.Rows(3).Cells(3).Value = 0
                    Else
                        DataGridView1.Rows(0).Cells(3).Value = ""
                        DataGridView1.Rows(1).Cells(3).Value = ""
                        DataGridView1.Rows(2).Cells(3).Value = ""
                        DataGridView1.Rows(3).Cells(3).Value = ""
                    End If
                    '1
                    If (cekSatu = True) Then
                        DataGridView1.Rows(0).Cells(4).Value = 1
                        DataGridView1.Rows(1).Cells(4).Value = 1
                        DataGridView1.Rows(2).Cells(4).Value = 1
                        DataGridView1.Rows(3).Cells(4).Value = 1
                    Else
                        DataGridView1.Rows(0).Cells(4).Value = ""
                        DataGridView1.Rows(1).Cells(4).Value = ""
                        DataGridView1.Rows(2).Cells(4).Value = ""
                        DataGridView1.Rows(3).Cells(4).Value = ""
                    End If
                    'Hasil
                    For i = 1 To 4
                        DataGridView1.Rows(i - 1).Cells(5).Value = Mid((Calculate(result)), i, 1)
                    Next

                ElseIf (cekB = True And cekC = True) Then
                    'A
                    DataGridView1.Rows(0).Cells(0).Value = ""
                    DataGridView1.Rows(1).Cells(0).Value = ""
                    DataGridView1.Rows(2).Cells(0).Value = ""
                    DataGridView1.Rows(3).Cells(0).Value = ""
                    'B
                    DataGridView1.Rows(0).Cells(1).Value = 1
                    DataGridView1.Rows(1).Cells(1).Value = 1
                    DataGridView1.Rows(2).Cells(1).Value = 0
                    DataGridView1.Rows(3).Cells(1).Value = 0
                    'C
                    DataGridView1.Rows(0).Cells(2).Value = 1
                    DataGridView1.Rows(1).Cells(2).Value = 0
                    DataGridView1.Rows(2).Cells(2).Value = 1
                    DataGridView1.Rows(3).Cells(2).Value = 0
                    '0
                    If (cekNol = True) Then
                        DataGridView1.Rows(0).Cells(3).Value = 0
                        DataGridView1.Rows(1).Cells(3).Value = 0
                        DataGridView1.Rows(2).Cells(3).Value = 0
                        DataGridView1.Rows(3).Cells(3).Value = 0
                    Else
                        DataGridView1.Rows(0).Cells(3).Value = ""
                        DataGridView1.Rows(1).Cells(3).Value = ""
                        DataGridView1.Rows(2).Cells(3).Value = ""
                        DataGridView1.Rows(3).Cells(3).Value = ""
                    End If
                    '1
                    If (cekSatu = True) Then
                        DataGridView1.Rows(0).Cells(4).Value = 1
                        DataGridView1.Rows(1).Cells(4).Value = 1
                        DataGridView1.Rows(2).Cells(4).Value = 1
                        DataGridView1.Rows(3).Cells(4).Value = 1
                    Else
                        DataGridView1.Rows(0).Cells(4).Value = ""
                        DataGridView1.Rows(1).Cells(4).Value = ""
                        DataGridView1.Rows(2).Cells(4).Value = ""
                        DataGridView1.Rows(3).Cells(4).Value = ""
                    End If
                    'Hasil
                    For i = 1 To 4
                        DataGridView1.Rows(i - 1).Cells(5).Value = Mid((Calculate(result)), i, 1)
                    Next

                End If
            ElseIf (cekA = True Or cekB = True Or cekC = True) Then
                DataGridView1.ColumnCount = 6
                DataGridView1.Columns(0).Name = "A"
                DataGridView1.Columns(1).Name = "B"
                DataGridView1.Columns(2).Name = "C"
                DataGridView1.Columns(3).Name = "0"
                DataGridView1.Columns(4).Name = "1"
                DataGridView1.Columns(5).Name = "Hasil"
                For j = 1 To 1
                    DataGridView1.Rows.Add()
                Next

                If (cekA = True) Then
                    'A
                    DataGridView1.Rows(0).Cells(0).Value = 1
                    DataGridView1.Rows(1).Cells(0).Value = 0
                    DataGridView1.Rows(0).Cells(1).Value = ""
                    DataGridView1.Rows(1).Cells(1).Value = ""
                    DataGridView1.Rows(0).Cells(2).Value = ""
                    DataGridView1.Rows(1).Cells(2).Value = ""
                    '0
                    If (cekNol = True) Then
                        DataGridView1.Rows(0).Cells(3).Value = 0
                        DataGridView1.Rows(1).Cells(3).Value = 0
                    Else
                        DataGridView1.Rows(0).Cells(3).Value = ""
                        DataGridView1.Rows(1).Cells(3).Value = ""
                    End If
                    '1
                    If (cekSatu = True) Then
                        DataGridView1.Rows(0).Cells(4).Value = 1
                        DataGridView1.Rows(1).Cells(4).Value = 1
                    Else
                        DataGridView1.Rows(0).Cells(4).Value = ""
                        DataGridView1.Rows(1).Cells(4).Value = ""
                    End If
                    For i = 1 To 2
                        DataGridView1.Rows(i - 1).Cells(5).Value = Mid((Calculate(result)), i, 1)
                    Next
                ElseIf (cekB = True) Then
                    'B
                    DataGridView1.Rows(0).Cells(0).Value = ""
                    DataGridView1.Rows(1).Cells(0).Value = ""
                    DataGridView1.Rows(0).Cells(1).Value = 1
                    DataGridView1.Rows(1).Cells(1).Value = 0
                    DataGridView1.Rows(0).Cells(2).Value = ""
                    DataGridView1.Rows(1).Cells(2).Value = ""
                    '0
                    If (cekNol = True) Then
                        DataGridView1.Rows(0).Cells(3).Value = 0
                        DataGridView1.Rows(1).Cells(3).Value = 0
                    Else
                        DataGridView1.Rows(0).Cells(3).Value = ""
                        DataGridView1.Rows(1).Cells(3).Value = ""
                    End If
                    '1
                    If (cekSatu = True) Then
                        DataGridView1.Rows(0).Cells(4).Value = 1
                        DataGridView1.Rows(1).Cells(4).Value = 1
                    Else
                        DataGridView1.Rows(0).Cells(4).Value = ""
                        DataGridView1.Rows(1).Cells(4).Value = ""
                    End If
                    'Hasil
                    For i = 1 To 2
                        DataGridView1.Rows(i - 1).Cells(5).Value = Mid((Calculate(result)), i, 1)
                    Next

                ElseIf (cekC = True) Then
                    'C
                    DataGridView1.Rows(0).Cells(0).Value = ""
                    DataGridView1.Rows(1).Cells(0).Value = ""
                    DataGridView1.Rows(0).Cells(1).Value = ""
                    DataGridView1.Rows(1).Cells(1).Value = ""
                    DataGridView1.Rows(0).Cells(2).Value = 1
                    DataGridView1.Rows(1).Cells(2).Value = 0
                    '0
                    If (cekNol = True) Then
                        DataGridView1.Rows(0).Cells(3).Value = 0
                        DataGridView1.Rows(1).Cells(3).Value = 0
                    Else
                        DataGridView1.Rows(0).Cells(3).Value = ""
                        DataGridView1.Rows(1).Cells(3).Value = ""
                    End If
                    '1
                    If (cekSatu = True) Then
                        DataGridView1.Rows(0).Cells(4).Value = 1
                        DataGridView1.Rows(1).Cells(4).Value = 1
                    Else
                        DataGridView1.Rows(0).Cells(4).Value = ""
                        DataGridView1.Rows(1).Cells(4).Value = ""
                    End If
                    'Hasil
                    For i = 1 To 2
                        DataGridView1.Rows(i - 1).Cells(5).Value = Mid((Calculate(result)), i, 1)
                    Next
                End If
            ElseIf (cekNol = True And cekA = False And cekB = False And cekC = False And cekSatu = False) Then
                DataGridView1.ColumnCount = 6
                DataGridView1.Columns(0).Name = "A"
                DataGridView1.Columns(1).Name = "B"
                DataGridView1.Columns(2).Name = "C"
                DataGridView1.Columns(3).Name = "0"
                DataGridView1.Columns(4).Name = "1"
                DataGridView1.Columns(5).Name = "Hasil"

                'DataGridView1.Rows.Add()
                'A
                DataGridView1.Rows(0).Cells(0).Value = ""
                'DataGridView1.Rows(1).Cells(0).Value = ""
                'DataGridView1.Rows(2).Cells(0).Value = ""
                'DataGridView1.Rows(3).Cells(0).Value = ""
                'B
                DataGridView1.Rows(0).Cells(1).Value = ""
                'DataGridView1.Rows(1).Cells(1).Value = ""
                'DataGridView1.Rows(2).Cells(1).Value = ""
                'DataGridView1.Rows(3).Cells(1).Value = ""
                'C
                DataGridView1.Rows(0).Cells(2).Value = ""
                'DataGridView1.Rows(1).Cells(2).Value = 0
                'DataGridView1.Rows(2).Cells(2).Value = 1
                'DataGridView1.Rows(3).Cells(2).Value = 0
                '0
                DataGridView1.Rows(0).Cells(3).Value = 0
                'DataGridView1.Rows(1).Cells(3).Value = 0
                'DataGridView1.Rows(2).Cells(3).Value = 0
                'DataGridView1.Rows(3).Cells(3).Value = 0
                '1
                DataGridView1.Rows(0).Cells(4).Value = ""
                'Hasil
                DataGridView1.Rows(0).Cells(5).Value = Mid((Calculate(result)), 1, 1)
            End If

            Dim akhir As String = ""
            Dim count0 As Integer = 0
            Dim count1 As Integer = 0
            Dim wow As Integer = 0
            Dim wih As String = ""
            wih = Calculate(result)
            wow = Len(wih)
            For i = 1 To wow
                akhir = Mid(wih, i, 1)
                If (akhir = "0") Then
                    count0 += 1
                ElseIf (akhir = "1") Then
                    count1 += 1
                End If
            Next
            Dim message As String = ""
            If (count0 = wow) Then
                message = "Kontradiksi"
                TextBox3.Text = message
            ElseIf (count1 = wow) Then
                message = "Tautologi"
                TextBox3.Text = message
            End If
        Next
    End Sub
    Private Sub TextBox3_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox3.TextChanged

    End Sub

    Private Sub Form1_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class