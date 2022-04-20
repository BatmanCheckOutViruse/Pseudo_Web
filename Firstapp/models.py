from django.db import models
class Feature(models.Model):

        other = models.CharField(max_length=1000000) #There are many different field
        code = models.CharField(max_length=1000000)


# Create your models here.