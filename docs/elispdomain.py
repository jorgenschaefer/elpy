# -*- coding: utf-8 -*-
"""
Emacs Lisp Domain for Sphinx

Copyright 2014 by Jorgen Schäfer

"""

from sphinx import addnodes
from sphinx.domains import Domain, ObjType
from sphinx.locale import l_
from sphinx.directives import ObjectDescription
from sphinx.roles import XRefRole
from sphinx.util.nodes import make_refnode


class ELispMarkup(ObjectDescription):
    def add_target_and_index(self, name, sig, signode):
        targetname = self.objtype + '-' + name
        if targetname not in self.state.document.ids:
            signode['names'].append(targetname)
            signode['ids'].append(targetname)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)

            objects = self.env.domaindata['el']['objects']
            key = (self.objtype, name)
            if key in objects:
                self.state_machine.reporter.warning(
                    'duplicate description of %s %s, ' % (self.objtype, name) +
                    'other instance in ' + self.env.doc2path(objects[key]),
                    line=self.lineno)
            objects[key] = self.env.docname
        indextext = self.get_index_text(self.objtype, name)
        if indextext:
            self.indexnode['entries'].append(('single', indextext,
                                              targetname, ''))


class ELispFunction(ELispMarkup):
    def get_index_text(self, objectname, name):
        return "{} (function)".format(name)

    def handle_signature(self, sig, signode):
        if sig.startswith("("):
            sig = sig[1:]
        if sig.endswith(")"):
            sig = sig[:-1]
        name, args = sig.split(" ", 1)

        params = addnodes.desc_parameterlist()
        params.child_text_separator = " "
        if args:
            params += addnodes.desc_name(name, name + " ")
        else:
            params += addnodes.desc_name(name, name)
        for arg in args.split():
            params += addnodes.desc_parameter(name, name)
        signode += params
        return name


class ELispVariable(ELispMarkup):
    def get_index_text(self, objectname, name):
        return "{} (variable)".format(name)

    def handle_signature(self, sig, signode):
        signode += addnodes.desc_name(sig, sig)
        return sig


class ELispCommand(ELispMarkup):
    option_spec = {
        'kbd': lambda x: x
    }

    def get_index_text(self, objectname, name):
        return "{} (command)".format(name)

    def handle_signature(self, sig, signode):
        kbd = self.options.get("kbd")
        name = sig
        if kbd:
            description = "{} ({})".format(kbd, name)
        else:
            description = "M-x {}".format(sig)

        signode += addnodes.desc_name(description, description)
        return name


class ELispOption(ELispMarkup):
    def get_index_text(self, objectname, name):
        return "{} (customize option)".format(name)

    def handle_signature(self, sig, signode):
        signode += addnodes.desc_name(sig, sig)
        signode += addnodes.desc_annotation(" (Customize Option)",
                                            " (Customize Option)")
        return sig


class ELispDomain(Domain):
    """Emacs Lisp language domain."""
    name = 'el'
    label = 'ELisp'

    object_types = {
        'function': ObjType(l_('function'), 'function'),
        'variable': ObjType(l_('variable'), 'variable'),
        'command': ObjType(l_('function'), 'command'),
        'option': ObjType(l_('variable'), 'option'),
    }
    directives = {
        'function': ELispFunction,
        'variable': ELispVariable,
        'command': ELispCommand,
        'option': ELispOption,
    }
    roles = {
        'function': XRefRole(),
        'variable': XRefRole(),
        'command': XRefRole(),
        'option': XRefRole(),
    }
    initial_data = {
        'objects': {},  # fullname, type -> docname
    }

    def clear_doc(self, docname):
        for (fullname, type_), docname in self.data['objects'].items():
                if fullname == docname:
                    del self.data['objects'][fullname]

    def resolve_xref(self, env, fromdocname, builder, type_, target, node,
                     contnode):
        objects = self.data['objects']
        objtypes = self.objtypes_for_role(type_)
        for objtype in objtypes:
            if (objtype, target) in objects:
                return make_refnode(builder, fromdocname,
                                    objects[objtype, target],
                                    objtype + "-" + target,
                                    contnode, target + " " + objtype)

    def get_objects(self):
        for (name, type_), docname in self.data['objects'].items():
            yield name, name, type_, docname, type_ + '-' + name, 1


def setup(app):
    app.add_domain(ELispDomain)
