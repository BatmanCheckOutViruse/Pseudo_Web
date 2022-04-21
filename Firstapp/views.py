from django.shortcuts import render
import sys
from io import StringIO
from django.core.mail import send_mail
from django.conf import settings
from django.http import HttpResponse
from .scriptsss import Execute
from .models import Feature
import os
from pathlib import Path

from django.http import HttpResponseRedirect

from .scriptsss import strings_with_arrows
def create(request):
    if request.method == "POST":
        other = request.POST['other']
        code = request.POST['code']
        new_Feature = Feature(other= other, code=code)
        new_Feature.save()
        python_error = False

        error = None
        cur_path = os.path.dirname(__file__)
        new_path = os.path.join(cur_path, '..\\static\\input.txt')
        with open(new_path, "w") as f:
            f.write(other)
        f.close()
        code = code.replace("“", '"')
        code = code.replace("”", '"')


        # check if there is anything in the text
        # if text.strip() == "" :
        cur_path = os.path.dirname(__file__)
        new_path = os.path.join(cur_path, '..\\static\\output.txt')
        try:
            wait_input = False
            result, error = Execute.run(code)
        except SystemExit:
            wait_input = True
            with open(new_path, 'a') as f:
               f.write("Program is waiting for input....")
            f.close()
        
        except:
            python_error = True
            return_val = "Error happens in the python code , please submit an error form and contact the developers"
            return HttpResponse(return_val)
        
        with open(new_path, 'r') as f:
            lines = f.readlines()
            if lines != []:
                lines[-1] = str(lines[-1][0:-1]) #since we append an additional newline a execute_input
        f.close()

        return_val ='<br>'.join(lines)
        if error != None:
            error_str = error.as_string()
            error_str = '<br>'.join(error_str.split('\n'))
            return_val = return_val + '<br>'+error_str
        if wait_input == False and python_error == False:
            return_val = return_val + '<br>' + "Program successfully executed"
        return HttpResponse(return_val)

def hi(request):
    context = {
       "name" : "patrick",
        "age" : 23
    }
    return render(request, 'Firstapp/hi.html')
    #return render(request, 'Firstapp/hi.html' , varaible, {'variable'} = variable )
def show_syntax(request):
    return render(request, 'show_syntax.html')
def submit_error(request):
    if request.method == 'POST':
        Errorinput = request.POST['Errorinput']
        Errorcode = request.POST['Errorcode']
        description = request.POST['dc']
        message = f"Input : {Errorinput} \n Errorcode : {Errorcode} \n description : {description} \n"
        send_mail("Contact form",
                  message,
                settings.EMAIL_HOST_USER,
                  ["elizaleung11@gmail.com"],
                  fail_silently=False)
        return_val =  '<br>' + "Form submitted :>" +'<br>' +'<br>'+"<a href=\"home-page\">Cheatsheet</a>"
        return HttpResponse(return_val)
    return render(request, 'submit_error.html')
