import lwz

def main():
    LWZ_encoder = lwz.lwz_encoder()
    seq = "tobeornottobeortobeornot"

    result = LWZ_encoder.encode(seq)

    print(result)
    print(LWZ_encoder.dict)

    LWZ_decoder = lwz.lwz_encoder()
    decode_result = LWZ_decoder.decode(result)

    print(decode_result)










if __name__ == "__main__":
    main()