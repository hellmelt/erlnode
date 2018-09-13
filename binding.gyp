{
  "targets": [
    {
      "target_name": "erlnode",

      "sources": [
        "src/binding.cpp",
        "src/erlnode.cpp"
      ],

      "include_dirs": [
        "<!@(node -p \"require('node-addon-api').include\")",
      ],
      "dependencies": [
      "<!(node -p \"require('node-addon-api').gyp\")"
      ],
      "defines": [ 'NAPI_DISABLE_CPP_EXCEPTIONS' ],
      "conditions": 	[
        ['OS == "mac"',
        {
          'ccflags': [
            '-mmacosx-version-min=10.13',
            '-std=c++11',
            '-stdlib=libc++'
          ],
          'include_dirs': [
            '<!(ls -d $ERL_TOP/lib/erl_interface*)/include',
            '<!(ls -d $ERL_TOP/lib/erl_interface*)/src/misc',
          ],
          'libraries': [
          '<!(ls -d $ERL_TOP/lib/erl_interface*)/lib/libei.a',
          '<!(ls -d $ERL_TOP/lib/erl_interface*)/lib/libei_st.a',
          '<!(ls -d $ERL_TOP/lib/erl_interface*)/lib/liberl_interface.a',
          '<!(ls -d $ERL_TOP/lib/erl_interface*)/lib/liberl_interface_st.a',
          ]
        }
      ]
      ],
    }
  ]
}