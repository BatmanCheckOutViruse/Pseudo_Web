from SDDWeb.Firstapp.scriptsss import Execute

run_count = 0

#try:
with open(r"C:\Users\eliza\PycharmProjects\SDDMain\RunText.txt", encoding="utf-8") as f:
        text = f.read()
        f.close()
        input_ = ""
        text = text.replace("“", '"')
        text = text.replace("”", '"')
        # check if there is anything in the text
        # if text.strip() == "" :
        result, error = Execute.run('stdin', text, input_)

        if error:
            print(error.as_string())

#except Exception as e: #
    #print(f"Runtime error in Program:  {e} , please contact developer to fix this")
