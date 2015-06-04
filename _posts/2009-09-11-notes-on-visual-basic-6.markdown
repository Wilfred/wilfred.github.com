--- 
layout: post
title: "Notes On Visual Basic 6"
---

VB6, like IE6 and household damp, doesn't seem to go away easily. I had the interesting experience recently of writing a medium size piece of code in VB6, and I've documented how I reduced inconveniences.

<h3>Setting up a less-pain environment</h3>

The VB6 editor does not go out of its way to help you. Perhaps its greatest offence is only having 5 steps of undo. Fortunately, there's a great <a href="http://www.emacswiki.org/cgi-bin/wiki/visual-basic-mode.el">VB6 mode for emacs</a>. It's also worth mentioning <a href="http://www.mztools.com/v3/mztools3.aspx">MZ-Tools</a>, a free add-on which gives a number of useful features. Two things I found particularly helpful were code analysis (to find variables and functions which are unused) and file reload, which makes external editing easier.

Also, the VB6 IDE doesn't support the mouse wheel. You will want the <a>hot fix for this</a>.

<h3>The language itself</h3>

MSDN seems to have been delibarately purged of VB6 documentation. For the initial getting up to speed in the language, there is an <a href="http://en.wikibooks.org/wiki/Visual_Basic">excellent wikibook</a>. There's also some useful learning material <a href="http://www.vb6.us/">here</a>, but there is substantial duplication between articles. Beyond this you'll need to obtain the MSDN disks of the correct vintage (2001 I believe) and hit F1 with the function selected (the search facility leaves something to be desired).

At this point its worth mentioning VB.Net. VB.Net has had many of kinks worked out of it, so it's a substantially different language. Migration to the .Net is definitely desirable, since support ended March 2008, but many parts of old VB6 code will need to be rewritten since the migration wizard can only do so much. Do not underestimate this task. Although the IDE is flaky on Windows Vista, the runtime is supported up to and including Windows 7. Legacy code will be around for a while yet. Note many of the curiosities listed below have been fixed in VB.Net, but <a href="http://www.codinghorror.com/blog/archives/000235.html"> most ex-VB programmers are looking elsewhere now</a>.

<h3>Some language quirks</h3>

{% highlight vbnet %}
' apostrophes denote comments
REM keyword also does this

' VB6 is case insensitive

Option Explicit
' this ensures that you only use variables you have declared
' (avoiding typos creating new variables)

Dim X, Y, Z As Integer
' Integers are only 16-bit. Use Long instead
' X and Y are Variants, only Z is an integer

If X = 1 Then
    X = 2 ' = is overloaded for comparison and assignment
   Dim S As Long
End If
' variables have only function/sub level scope. S is still
' accessible here
S = 0

' null keyword is Nothing

Dim AnArray(3) As Boolean
' this creates a FOUR element array numbered 0-3
Dim AnotherArray(-5 To 10)
' we can also define arbitrary array bases

Call DoStuff(X, Y, Z) ' call keyword requires brackets
DoStuff X, Y, Z ' without call we may not use brackets
S = DoStuff(X, Y, Z) ' unless we are returning a value

Dim Obj As Object
Set Object = GetSomeObject("foo") ' set is compulsory for reference assignment
Let X = 2 ' let is completely optional for primitives

Dim foo As String
foo = "escape strings with "" (double quotation marks)"
foo = "append with ampersand " &amp; foo

X = 1 / 2 ' floating point division
X = 3 \ 2 ' integer division

Private Sub Foo(ByVal A As Integer, ByRef B As String)
    ' arguments are passed by reference as default!

    On Error GoTo Panic
    ' yes, goto is available

    If A <>; 0 And Test(A) Then ' not equal to zero
       ' Boolean logic does not use short circuiting
       ' so Test(A) is always evaluated
       Exit Sub
    End If

Panic:
    Debug.Print "Do you know where your towel is?"
End Sub

Function Bar() As Integer
    ' subs may not return values
   ' they are optional for functions
End Function

Public Sub Thing_CrazyEvent() As Integer
    ' by convention this code is called when the CrazyEvent event
   ' occurs to Thing

    Dim i As Long
    ' vb6 ide has a habit of changing the capitalisation based on
   ' variable names elsewhere
   For I = 2 To 20 Step 2
        MsgBox I
    Next I ' stating variable here is optional but
   ' if you do the compiler will check it

    Dim N As ListItem
    For Each N In List
        ' for each loops are also available
        DoSomething N
    Next
End Function

Private Sub Something(A As String, B As String, C As String, _
                      D As String)
    ' line continuations are available using _ but they are capped at 25 lines
   ' this is a pain for sql, see
   ' dailydoseofexcel.com/archives/2005/04/18/line-continuation-limit/
End Sub

Implements AnInterfaceName
' vb objects can only use interface inheritance
' not implementation inheritance

' objects also do not support constructor methods
{% endhighlight %}

<h3>Other Tools</h3>

Testing tools are few and far between for VB6. For unit testing there is the open source <a href="http://simplyvbunit.sourceforge.net/">SimplyVBUnit</a>. I had some success with it, but you'll need to examine the sample code as the documentation is rather lacking. It requires your code to be structured with a DLL server backend to a GUI frontend, which is different to a lot of legacy code. If you can get this structure you will find piecewise migration to VB.Net a lot less painful though. For GUI testing your options are even more limited, so your best option is probably a good <a href="https://www.autoitscript.com/site/autoit/">generic GUI scripter</a>.

Finally, beware Visual SourceSafe. It can <a href="http://msdn.microsoft.com/en-us/library/ms181079%28VS.80%29.aspx">corrupt your code history if you have too much data</a>. Its weaknesses are expounded <a href="http://www.highprogrammer.com/alan/windev/sourcesafe.html">here</a>. <a href="http://subversion.tigris.org/">Subversion</a> is probably the best substitute as it offers mature GUI replacements for Sourcesafe's functionality. Integrating subversion with the VB6 IDE is tricky, there is only <a href="http://svnvb6.tigris.org/">one piece of software</a> that enables this. Alternatively <a href="http://tortoisesvn.tigris.org/">TortoiseSVN</a> will get the job done without the tight integration. Migration of code history is a sufficient pain that sometimes it's easier just to check in the current build. If you want to go the automated migration route then <a href="http://www.pumacode.org/projects/vss2svn">Vss2Svn</a> is the most mature tool available.
