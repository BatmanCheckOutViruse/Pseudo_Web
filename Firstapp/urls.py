from django.urls import path
from . import  views
urlpatterns = [
    path('home-page', views.hi , name="home-page"),
    path('submit_error',views.submit_error, name= 'counter'),
    path('show_syntax',views.show_syntax, name= 'show_syntax'),
    path('create', views.create, name="create")
]