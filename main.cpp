#include <iostream>
#include <string>
#include <vector>
#include <fstream>

//#define PRINTREFS

void assert(bool x) {
	if (!x) {
		std::cerr << "!!!!";
		exit(-1);
	}
}

class term {
	int level, reflevel;
	char name;
	std::vector<term> childs;

	int type() const {
		return reflevel >= -1 ? 0 : reflevel == -3 ? 1 : 2;
	}
	//L is level of the top
	void updateLevel(int delta, int L) {
		int T = type();
		if (T == 0) {
			//std::cout << (char)name << ":" << level << ":" << reflevel << ":" << L << "\n";
			if (reflevel >= L) reflevel += delta;
		} else {
			for (int i = 0, n = childs.size(); i < n; i++) {
				childs[i].updateLevel(delta, L);
			}
		}
		level += delta;
	}
	void link(int lambdalevel, char x) {
		int T = type();
		if (T == 0) {
			if (name == x && reflevel == -1)reflevel = lambdalevel;
		} else {
			for (int i = 0, n = childs.size(); i < n; i++) {
				childs[i].link(lambdalevel, x);
			}
		}
	}
	void replace(int lambdalevel, char x, const term& t) {
		int T = type();
		if (T == 0) {
			if (name == x && reflevel == lambdalevel) {
				int l0 = level;
				*this = t;
				//std::cout << "   leaf: " << *this;
				updateLevel(l0 - t.level, t.level);
				//std::cout << "  >leaf: " << *this;
			}
		} else {
			for (int i = 0, n = childs.size(); i < n; i++) {
				childs[i].replace(lambdalevel, x, t);
			}
		}
	}
public:
	term() :level(0), reflevel(-2), name(0), childs({}) { }
	explicit term(char x) :level(0), reflevel(-1), name(x), childs() { }
	term(char x, const term& E) :level(0), reflevel(-3), name(x), childs({E}) {
		assert(E.level == 0);
		childs[0].updateLevel(1, 0);
		link(level, name);
	}
	term(const term& E1, const term& E2) :level(0), reflevel(-2), name(0) {
		assert(E1.level == 0 && E2.level == 0);
		if (E1.type() == 2) {
			int s = E1.childs.size();
			childs.insert(childs.begin(), E1.childs.begin(), E1.childs.end());
			childs.push_back(E2);
			childs[s].updateLevel(1, 0);
		} else {
			childs = {E1, E2};
			childs[0].updateLevel(1, 0);
			childs[1].updateLevel(1, 0);
		}
	}

	static term line(const std::string& s) {
		assert(s.size() > 0);
		term L;
		for (int i = 0, n = s.size(); i < n; i++) {
			term t(s[i]);
			t.updateLevel(1, 0);
			L.childs.push_back(t);
		}
		return L;
	}
	static term rLine(const std::string& s) {
		assert(s.size() > 0);
		int n = s.size();
		term L(s[n - 1]);
		for (int i = n - 2; i >= 0; i--) {
			term T(term(s[n - 1]), L);
			L = T;
		}
		return L;
	}
	static term multiarg(const std::string& s, term E) {
		term temp;
		for (int i = s.size() - 1; i >= 0; i--) {
			temp = term(s[i], E);
			E = temp;
		}
		return E;
	}

	static std::string charWithStrokes(char c, int n) {
		std::string s = {c};
#ifdef PRINTREFS
		if (n > 0)s += '\'';
		if (n > 1)s += std::to_string(n);
#endif
		return s;
	}
	std::string asString() const {
		int T = type();
		if (T == 0) {
			return charWithStrokes(name, reflevel >= 0 ? level - reflevel : 0);
		} else if (T == 1) {
			std::string s = "L";
			const term* t = this;
			while (t->type() == 1) {
				s += std::string({t->name});
				t = &(t->childs[0]);
			}
			return s + "." + t->asString();
		} else {
			std::string ans = "(";
			for (int i = 0, n = childs.size(); i < n; i++) {
				ans += childs[i].asString();
			}
			return ans + ")";
		}
	}
	friend std::ostream& operator<<(std::ostream& s, const term& t) {
		return s << t.asString() << "\n";
	}
	term operator^(const term& other) const {
		return term(*this, other);
	}

	bool reduce() {
		int T = type();
		if (T == 0) {
			return false;
		} else if (T == 1) {
			return childs[0].reduce();
		} else {
			int s = childs.size();
			if (s == 1) {
				term temp = childs[0];
				*this = temp;
				updateLevel(-1, level);
			}
			term& L = childs[0];
			term& E = childs[1];
			if (L.type() == 1) {

				L.childs[0].replace(L.level, L.name, E);
				term temp = L.childs[0];
				L = temp;
				L.updateLevel(-1, L.level);

				childs.erase(++childs.begin());

				if (s == 2) {
					temp = childs[0];
					*this = temp;
					updateLevel(-1, level);
				}
				return true;
			}
			for (int i = 0; i < s; i++) {
				if (childs[i].reduce()) return true;
			}
		}
		return false;
	}
	term reduced(int n = 1000) const {
		term t = *this;
		for (int i = 0; i < n && t.reduce(); i++);
		return t;
	}
	void fullreduce(int n = 1000, std::ostream& s = std::cout) {
		do {
			s << *this;
		}
		while (reduce() && n-- > 0);
	}

	void rename(char from, int L, char to) {
		int T = type();
		if (T == 0) {
			if (name == from && reflevel == L) {
				name = to;
			}
		} else {
			for (auto& c : childs) {
				c.rename(from, L, to);
			}
		}
	}
	void renameT1(char to) {
		assert(type() == 1);
		rename(name, level, to);
		name = to;
	}
	void rename(int L = 0) {
		int T = type();
		if (T == 1) {
			renameT1((char)('a' + L));
		}
		if (T > 0) {
			for (auto& c : childs) {
				c.rename(L + 1);
			}
		}
	}

};
namespace ct {
	const term I('a', term('a'));
	const term K = term::multiarg("ab", term('a')), T = K;
	const term KI = term::multiarg("ab", term('b')), F = KI;
	const term M = term('x', term::line("xx"));
	const term C = term::multiarg("xyz", term::line("xzy"));
	const term OMEGA = M ^ M;

	term NUMBER(uint n) {
		term A('x'), temp;
		for (int i = 0; i < n; i++) {
			temp = term(term('f'), A);
			A = temp;
		}
		return term::multiarg("fx", A);
	}
	term SUCC = term::multiarg("wyx", term('y') ^ term::line("wyx"));
	term PRED = term::multiarg("nfx",
			term('n') ^
					term::multiarg("gh",
							term('h') ^
									term::line("gf")) ^
					term('u', term('x')) ^
					I);
	term PLUS = term::multiarg("mnfx", term::line("mf") ^ term::line("nfx"));
	term MULT = term::multiarg("mnf", term('m') ^ term::line("nf"));
	term POW = term::multiarg("be", term::line("eb"));

	term AND = term::multiarg("qp", term::line("pqp"));
	term OR = term::multiarg("qp", term::line("ppq"));
	term NOT = C;
	term IF = term::multiarg("pab", term::line("pab"));

	term ISZERO = term('n', term('n') ^ term('x', F) ^ T);

	term Y = term('g', term('x', term('g') ^ term::line("xx")) ^ term('x', term('g') ^ term::line("xx")));

}

int main() {

	{
		term A = ct::NUMBER(5);
		term B = ct::I;
		term C = ct::NUMBER(0);
		std::cout << (ct::ISZERO ^ A).reduced();
		std::cout << (ct::ISZERO ^ C).reduced();
		std::cout << (ct::ISZERO ^ B).reduced();
		std::cout << "==============\n\n";
	}

	{
		auto A = ct::NUMBER(2), B = ct::NUMBER(2);
		std::cout << A;
		std::cout << B;
		std::cout << ct::MULT;
		auto ApB = term('y', ct::POW ^ A ^ B ^ term('y'));
		std::cout << ApB << ApB.reduced();
		std::cout << "==============\n\n";
	}

	{
		term Q = ct::IF ^ ct::T ^ ct::I ^ ct::OMEGA;
		std::cout << Q << Q.reduced(10);
		std::cout << "==============\n\n";
	}

	{
		term G = term::multiarg(
				"rn",
				ct::IF ^
						(ct::ISZERO ^ term('n')) ^
						ct::NUMBER(1) ^
						(ct::MULT ^ term('n') ^ (term('r') ^ (ct::PRED ^ term('n'))))
		);

		term prog = (ct::Y ^ G) ^ ct::NUMBER(1);
		std::cout << prog;
		std::ofstream fout("result.txt");
		prog.fullreduce(100000, fout);
		std::cout << prog;
		prog.rename();
		std::cout << prog;
	}




	//std::cout << ct::K;
	//term Q = ct::C * ct::KI;
	//std::cout << Q << Q.reduced();
	//term X =
	//term('y', term('z',
	//		term(term('x', term(term(term('x'), term('y')), term('z'))), term('a'))
	//))
	//;
	//term Q = term::multiarg("yx", term::line("xxyx"));

	//std::cout << Q.asString() << "\n";
	//term OMEGA = term(M, M);
	//std::cout << M.asString() << "\n" << OMEGA.asString() << "\n";
	//OMEGA.reduce();
	//std::cout << OMEGA.asString() << "\n";
//	X.reduce();
//	std::cout << X.asString() << "\n";
	//	term I = term(term('x', term(term('x'), term(term('x'), term('x')))), term('y'));
//	std::cout << I.asString() << "\n";
//	I.reduce();
//	std::cout << I.asString() << "\n";

	return 0;
}
