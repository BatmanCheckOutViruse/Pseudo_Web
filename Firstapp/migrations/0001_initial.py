# Generated by Django 4.0.3 on 2022-04-01 08:08

from django.db import migrations, models


class Migration(migrations.Migration):

    initial = True

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='Feature',
            fields=[
                ('id', models.BigAutoField(auto_created=True, primary_key=True, serialize=False, verbose_name='ID')),
                ('other', models.CharField(max_length=1000000)),
                ('code', models.CharField(max_length=1000000)),
            ],
        ),
    ]
