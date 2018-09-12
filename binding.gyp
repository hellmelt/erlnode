{
  "targets": [
    {
      "target_name": "erlnode",

      "sources": [
        "src/binding.cpp",
        "src/erlnode.cpp"
      ],

      'include_dirs': [
        "<!@(node -p \"require('node-addon-api').include\")",
        '/usr/local/Cellar/erlang/20.2.2/lib/erlang/lib/erl_interface-3.10.1/include',
        '/usr/local/Cellar/erlang/20.2.2/lib/erlang/lib/erl_interface-3.10.1/src/misc',
      ],
      'dependencies': [
      "<!(node -p \"require('node-addon-api').gyp\")"
      ],
      "libraries": [
        "/usr/local/Cellar/erlang/20.2.2/lib/erlang/lib/erl_interface-3.10.1/lib/libei.a",
        "/usr/local/Cellar/erlang/20.2.2/lib/erlang/lib/erl_interface-3.10.1/lib/libei_st.a",
        "/usr/local/Cellar/erlang/20.2.2/lib/erlang/lib/erl_interface-3.10.1/lib/liberl_interface.a",
        "/usr/local/Cellar/erlang/20.2.2/lib/erlang/lib/erl_interface-3.10.1/lib/liberl_interface_st.a",
      ],
      "defines": [ 'NAPI_DISABLE_CPP_EXCEPTIONS' ],
      "conditions": 	[
        ['OS == "mac"',
        {
          'ccflags': [
            '-mmacosx-version-min=10.13',
            '-std=c++11',
            '-stdlib=libc++'
          ]
        }
      ]
      ],
    }
  ]
}